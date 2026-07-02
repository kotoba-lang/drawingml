(ns drawingml.parse
  "Small DrawingML XML to EDN projection helpers."
  (:require [clojure.string :as str]))

(def emu-per-inch 914400)

(defn- parse-int-radix [s radix]
  #?(:clj (Integer/parseInt s radix)
     :cljs (js/parseInt s radix)))

(defn- codepoint-string [n]
  #?(:clj (String. (Character/toChars n))
     :cljs (.fromCodePoint js/String n)))

(defn- decode-numeric-entity
  "&#8226;/&#x2022; -> the literal character. Bullet characters (buChar) are
  routinely XML-escaped as numeric entities rather than the raw glyph."
  [[raw hex dec]]
  (try
    (codepoint-string (if hex (parse-int-radix hex 16) (parse-int-radix dec 10)))
    (catch #?(:clj Exception :cljs :default) _
      raw)))

(defn xml-unescape [s]
  (-> (str (or s ""))
      (str/replace #"&#x([0-9A-Fa-f]+);|&#([0-9]+);" decode-numeric-entity)
      (str/replace "&lt;" "<")
      (str/replace "&gt;" ">")
      (str/replace "&quot;" "\"")
      (str/replace "&apos;" "'")
      (str/replace "&amp;" "&")))

(defn xml-attr [xml attr]
  (some-> (second (re-find (re-pattern (str "\\b" attr "=\"([^\"]*)\"")) (or xml "")))
          xml-unescape))

(defn xml-texts [xml tag]
  (->> (re-seq (re-pattern (str "<" tag "\\b[^>]*>([\\s\\S]*?)</" tag ">")) (or xml ""))
       (map second)
       (map xml-unescape)
       (remove str/blank?)))

(defn first-xml-text [xml tag]
  (first (xml-texts xml tag)))

(defn xml-elements [xml tag]
  (re-seq (re-pattern (str "<" tag "\\b[\\s\\S]*?</" tag ">")) (or xml "")))

(defn parse-double-safe [x]
  (when-not (str/blank? (str x))
    (let [n #?(:clj (try (Double/parseDouble (str x))
                         (catch Exception _ nil))
               :cljs (js/parseFloat (str x)))]
      (when #?(:clj (and (some? n) (Double/isFinite n))
               :cljs (js/isFinite n))
        n))))

(defn emu->inch [n fallback]
  (if-let [value (parse-double-safe n)]
    (if (pos? value)
      (double (/ value emu-per-inch))
      fallback)
    fallback))

(defn shape-name [block idx fallback]
  (or (xml-attr (or (re-find #"<p:cNvPr\b[^>]*>" (or block "")) "") "name")
      (str fallback "-" (inc idx))))

(defn placeholder [block]
  (when-let [ph (re-find #"<p:ph\b[^>]*/?>" (or block ""))]
    (cond-> {}
      (xml-attr ph "type") (assoc :type (xml-attr ph "type"))
      (xml-attr ph "idx") (assoc :idx (xml-attr ph "idx"))
      (xml-attr ph "sz") (assoc :size (xml-attr ph "sz"))
      (xml-attr ph "orient") (assoc :orient (xml-attr ph "orient")))))

(defn- add-placeholder [shape block]
  (cond-> shape
    (placeholder block) (assoc :drawingml/placeholder (placeholder block))))

(defn- group-metadata [groups block]
  (some (fn [[idx group-block]]
          (when (and (not= group-block block)
                     (str/includes? group-block block))
            {:index idx
             :id (shape-name group-block idx "group")}))
        (map-indexed vector groups)))

(defn- add-group [shape group]
  (cond-> shape
    group (assoc :drawingml/group group)))

(defn- source-extras [shape]
  (cond-> {}
    (:drawingml/group shape) (assoc :ooxml/group (:drawingml/group shape))
    (:drawingml/placeholder shape) (assoc :ooxml/placeholder (:drawingml/placeholder shape))
    (:drawingml/chart-rel-id shape) (assoc :ooxml/chart-rel-id (:drawingml/chart-rel-id shape))
    (:drawingml/chart-part shape) (assoc :ooxml/chart-part (:drawingml/chart-part shape))
    (:drawingml/workbook-part shape) (assoc :ooxml/workbook-part (:drawingml/workbook-part shape))))

(defn- with-source [shape opts kind idx]
  (cond-> shape
    (:part opts)
    (assoc :ooxml/source (cond-> (merge {:ooxml/part (:part opts)
                                         :ooxml/kind kind
                                         :ooxml/index idx}
                                        (source-extras shape))
                           (:source opts) (assoc :ooxml/source (:source opts))))))

(def default-xfrm {:drawingml/x 0.8 :drawingml/y 0.8 :drawingml/w 8.4 :drawingml/h 0.7})

(defn xfrm-explicit
  "The shape's own <a:xfrm>/<p:xfrm> geometry, or nil when the block omits it.
  PowerPoint omits <a:xfrm> on placeholders that were never moved/resized and
  expects the slide layout/master geometry to apply instead."
  [block]
  (when-let [body (second (or (re-find #"<a:xfrm\b[^>]*>([\s\S]*?)</a:xfrm>" (or block ""))
                              (re-find #"<p:xfrm\b[^>]*>([\s\S]*?)</p:xfrm>" (or block ""))))]
    (let [off (or (re-find #"<a:off\b[^>]*>" body) "")
          ext (or (re-find #"<a:ext\b[^>]*>" body) "")]
      {:drawingml/x (emu->inch (xml-attr off "x") 0.8)
       :drawingml/y (emu->inch (xml-attr off "y") 0.8)
       :drawingml/w (emu->inch (xml-attr ext "cx") 8.4)
       :drawingml/h (emu->inch (xml-attr ext "cy") 0.7)})))

(defn placeholder-geometry-index
  "Indexes placeholder geometry from a slideLayout/slideMaster XML string,
  keyed by [idx type] (exact placeholder match) and [nil type] (type-only
  fallback, first match wins). Feeds `xfrm`'s :placeholder-geometry opt so a
  slide shape that omits <a:xfrm> can inherit its layout/master position."
  [xml]
  (reduce (fn [index block]
            (if-let [ph (placeholder block)]
              (if-let [geometry (xfrm-explicit block)]
                (let [type (:type ph "body")]
                  (cond-> index
                    (and (:idx ph) (not (contains? index [(:idx ph) type])))
                    (assoc [(:idx ph) type] geometry)

                    (not (contains? index [nil type]))
                    (assoc [nil type] geometry)))
                index)
              index))
          {}
          (xml-elements xml "p:sp")))

(defn merge-placeholder-geometry-indexes
  "Merges slideLayout and slideMaster placeholder geometry indexes, preferring
  the (more specific) layout entries."
  [layout-index master-index]
  (merge master-index layout-index))

;; The OOXML default colour map (no explicit <p:clrMap> override): bg1/tx1/
;; bg2/tx2 are the placeholder-facing aliases PowerPoint actually emits in
;; <a:schemeClr val="...">, resolved here to the theme's dk/lt slots.
(def ^:private default-color-map
  {:bg1 :lt1 :tx1 :dk1 :bg2 :lt2 :tx2 :dk2})

(defn scheme-color-role [xml]
  (some-> (second (re-find #"<a:schemeClr\b[^>]*\bval=\"([A-Za-z0-9]+)\"" (or xml "")))
          str/lower-case
          keyword
          ((fn [role] (get default-color-map role role)))))

(defn first-color
  ([xml] (first-color xml nil))
  ([xml theme-colors]
   (or (some-> (or (second (re-find #"<a:srgbClr\b[^>]*\bval=\"([0-9A-Fa-f]{6})\"" (or xml "")))
                   (second (re-find #"\blastClr=\"([0-9A-Fa-f]{6})\"" (or xml ""))))
               str/upper-case)
       (get theme-colors (scheme-color-role xml)))))

(defn solid-fill
  ([block] (solid-fill block nil))
  ([block theme-colors]
   (some-> (second (re-find #"<a:solidFill\b[^>]*>([\s\S]*?)</a:solidFill>" (or block "")))
           (first-color theme-colors))))

(defn line-fill
  ([block] (line-fill block nil))
  ([block theme-colors]
   (some-> (second (re-find #"<a:ln\b[^>]*>([\s\S]*?)</a:ln>" (or block "")))
           (first-color theme-colors))))

(defn- text-body [block]
  (second (or (re-find #"<p:txBody\b[^>]*>([\s\S]*?)</p:txBody>" (or block ""))
              (re-find #"<a:txBody\b[^>]*>([\s\S]*?)</a:txBody>" (or block "")))))

(defn text-color
  "A shape's text (run) color, scoped to its <p:txBody>/<a:txBody> only.
  Unlike `solid-fill` on the whole shape block, this never picks up the
  shape's own <p:spPr> fill — a non-:rect AutoShape (roundRect, oval, ...)
  that has both a themed fill and a run without its own explicit color would
  otherwise have its shape-fill color misattributed as its text color."
  ([block] (text-color block nil))
  ([block theme-colors]
   (some-> (text-body block) (solid-fill theme-colors))))

(defn xfrm
  "Shape geometry: explicit <a:xfrm> when present, else placeholder geometry
  inherited via opts' :placeholder-geometry index (built by
  `placeholder-geometry-index`/`merge-placeholder-geometry-indexes` from the
  slide's layout+master), else the historical fixed fallback."
  ([block] (xfrm block {}))
  ([block opts]
   (or (xfrm-explicit block)
       (when-let [ph (placeholder block)]
         (let [index (:placeholder-geometry opts)
               type (:type ph "body")]
           (or (get index [(:idx ph) type])
               (get index [nil type]))))
       default-xfrm)))

(defn font-size [block fallback]
  (if-let [sz (some-> (re-find #"<a:rPr\b[^>]*>" (or block "")) (xml-attr "sz") parse-double-safe)]
    (double (/ sz 100))
    fallback))

(defn- paragraph-text
  "A single <a:p>'s own text: its runs concatenated with no separator (they
  are contiguous, differently-styled spans of the same line)."
  [p-block]
  (str/join "" (xml-texts p-block "a:t")))

(defn paragraphs-text
  "Paragraph-aware text extraction: <a:p> elements joined by newline (each is
  its own line/bullet), while runs *within* a paragraph are concatenated
  without a separator. Prior flattening (every <a:t> in the block joined by
  newline) wrongly treated same-paragraph, differently-styled runs as
  separate lines. Falls back to that flat join when no <a:p> boundary is
  found (e.g. a bare fragment)."
  [block]
  (let [paragraphs (xml-elements block "a:p")]
    (if (seq paragraphs)
      (str/join "\n" (map paragraph-text paragraphs))
      (str/join "\n" (xml-texts block "a:t")))))

(defn- paragraph-pPr
  "A paragraph's own <a:pPr> element (open+children+close, or self-closing),
  or nil. algn lives as an attribute on the tag itself; bullet/line-spacing
  live as child elements -- both need the full tag text, not just the inner
  content, hence returning the whole match rather than a captured group."
  [p-block]
  (or (re-find #"<a:pPr\b[^>]*>[\s\S]*?</a:pPr>" (or p-block ""))
      (re-find #"<a:pPr\b[^>]*/>" (or p-block ""))))

(defn- paragraph-align [pPr]
  (when pPr
    (case (xml-attr pPr "algn")
      "ctr" :center
      "r" :right
      "just" :justify
      "l" :left
      nil)))

(defn- paragraph-bullet [pPr]
  (when pPr
    (cond
      (re-find #"<a:buNone\b" pPr) {:type :none}
      :else (or (when-let [ch (second (re-find #"<a:buChar\b[^>]*\bchar=\"([^\"]*)\"" pPr))]
                  {:type :char :char (xml-unescape ch)})
                (when-let [scheme (second (re-find #"<a:buAutoNum\b[^>]*\btype=\"([^\"]*)\"" pPr))]
                  {:type :auto-num :scheme scheme})))))

(defn- paragraph-line-spacing
  "A paragraph's line spacing as a multiplier (1.0 = single spacing), from
  <a:lnSpc><a:spcPct val=\"150000\"/></a:lnSpc> (val is a percentage x1000).
  <a:spcPts> (an absolute point size, not a multiplier) isn't convertible to
  a multiplier without knowing the run's font size, so it's left unread for
  now rather than guessed at."
  [pPr]
  (when pPr
    (some-> (second (re-find #"<a:lnSpc>\s*<a:spcPct\b[^>]*\bval=\"([0-9]+)\"" pPr))
            parse-double-safe
            (/ 100000.0))))

(defn paragraphs
  "Structured per-paragraph extraction (text + alignment + bullet + line-
  spacing), alongside (not replacing) the flattened text `paragraphs-text`
  produces. Lets a writer reconstruct real bullets/alignment/spacing instead
  of always emitting plain, unstyled lines -- the single biggest visible gap
  versus a real PowerPoint-authored deck (bulleted lists are near-universal)."
  [block]
  (vec
   (for [p-block (xml-elements block "a:p")
         :let [pPr (paragraph-pPr p-block)
               align (paragraph-align pPr)
               bullet (paragraph-bullet pPr)
               line-spacing (paragraph-line-spacing pPr)]]
     (cond-> {:text (paragraph-text p-block)}
       align (assoc :align align)
       bullet (assoc :bullet bullet)
       line-spacing (assoc :line-spacing line-spacing)))))

(defn table-rows
  "The table's cell grid as rows of paragraph-aware cell text, reading <a:tr>
  then <a:tc> in document order. Empty when the block has no rows."
  [block]
  (vec (for [row (xml-elements block "a:tr")]
         (vec (map paragraphs-text (xml-elements row "a:tc"))))))

(defn geometry [block]
  (some-> (re-find #"<a:prstGeom\b[^>]*>" (or block ""))
          (xml-attr "prst")
          keyword))

(defn text-shape
  ([idx block] (text-shape idx block {}))
  ([idx block opts]
   (let [texts (vec (xml-texts block "a:t"))
         text (paragraphs-text block)
         paras (paragraphs block)]
     (when-not (str/blank? text)
       (cond-> (add-placeholder
                (merge {:drawingml/id (shape-name block idx "text")
                        :drawingml/kind :text
                        :drawingml/text text
                        :drawingml/font-size (font-size block 20)
                        :drawingml/color (or (text-color block (:theme-colors opts)) "17202A")}
                       (xfrm block opts))
                block)
         (> (count texts) 1) (assoc :drawingml/source-kind :drawingml/text-runs)
         (seq paras) (assoc :drawingml/paragraphs paras))))))

(defn rect-shape
  ([idx block] (rect-shape idx block {}))
  ([idx block opts]
   (when (= :rect (geometry block))
     (cond-> (add-placeholder
              (merge {:drawingml/id (shape-name block idx "rect")
                      :drawingml/kind :rect
                      :drawingml/fill (or (solid-fill block (:theme-colors opts)) "EAF0F8")}
                     (xfrm block opts))
              block)
       (line-fill block (:theme-colors opts)) (assoc :drawingml/line (line-fill block (:theme-colors opts)))))))

(defn pic-shape
  ([idx block] (pic-shape idx block {}))
  ([idx block opts]
   (merge {:drawingml/id (shape-name block idx "pic")
           :drawingml/kind :pic
           :drawingml/text (shape-name block idx "Picture")
           :drawingml/source-kind :drawingml/pic
           :drawingml/font-size 12
           :drawingml/color "334155"}
          (xfrm block opts))))

(defn table-shape
  ([idx block] (table-shape idx block {}))
  ([idx block opts]
   (let [texts (vec (xml-texts block "a:t"))
         rows (table-rows block)]
     (when (seq texts)
       (cond-> (merge {:drawingml/id (shape-name block idx "table")
                       :drawingml/kind :table
                       :drawingml/text (if (seq rows)
                                         (str/join "\n" (mapcat identity rows))
                                         (str/join "\n" texts))
                       :drawingml/source-kind :drawingml/table
                       :drawingml/font-size 14
                       :drawingml/color "17202A"}
                      (xfrm block opts))
         (seq rows) (assoc :drawingml/rows rows))))))

(defn chart-shape
  ([idx block] (chart-shape idx block {}))
  ([idx block opts]
   (when-let [chart (re-find #"<c:chart\b[^>]*/?>" (or block ""))]
     (let [rel-id (xml-attr chart "r:id")
           rel (get (:rels opts) rel-id)]
       (cond-> (merge {:drawingml/id (shape-name block idx "chart")
                       :drawingml/kind :chart
                       :drawingml/text (shape-name block idx "Chart")
                       :drawingml/source-kind :drawingml/chart
                       :drawingml/font-size 12
                       :drawingml/color "334155"}
                      (xfrm block opts))
         rel-id (assoc :drawingml/chart-rel-id rel-id)
         (:target-path rel) (assoc :drawingml/chart-part (:target-path rel))
         (:workbook-path rel) (assoc :drawingml/workbook-part (:workbook-path rel)))))))

(defn graphic-frame-shape
  ([idx block] (graphic-frame-shape idx block {}))
  ([idx block opts]
   (or (table-shape idx block opts)
       (chart-shape idx block opts))))

(defn fallback-text-shapes [texts]
  (vec
   (map-indexed
    (fn [idx text]
      {:drawingml/id (str "text-" (inc idx))
       :drawingml/kind :text
       :drawingml/text text
       :drawingml/x 0.8
       :drawingml/y (+ 0.75 (* idx 0.72))
       :drawingml/w 8.4
       :drawingml/h 0.6
       :drawingml/font-size (if (zero? idx) 30 20)
       :drawingml/color (if (zero? idx) "17202A" "334155")})
    texts)))

(defn shapes
  ([xml] (shapes xml {}))
  ([xml opts]
  (let [groups (vec (xml-elements xml "p:grpSp"))
        shape-blocks (vec (xml-elements xml "p:sp"))
        graphic-frame-blocks (vec (xml-elements xml "p:graphicFrame"))
        table-blocks (vec (xml-elements xml "a:tbl"))
        parsed-shapes (vec (keep-indexed (fn [shape-idx block]
                                           (some-> (or (text-shape shape-idx block opts)
                                                       (rect-shape shape-idx block opts))
                                                   (add-group (group-metadata groups block))
                                                   (with-source opts :p/sp shape-idx)))
                                         shape-blocks))
        pics (vec (map-indexed (fn [idx block]
                                  (-> (pic-shape idx block opts)
                                      (add-group (group-metadata groups block))
                                      (with-source opts :p/pic idx)))
                                (xml-elements xml "p:pic")))
        graphic-frames (vec (keep-indexed (fn [idx block]
                                             (some-> (graphic-frame-shape idx block opts)
                                                     (add-group (group-metadata groups block))
                                                     (with-source opts :p/graphicFrame idx)))
                                           graphic-frame-blocks))
        standalone-tables (vec (keep-indexed
                                (fn [idx block]
                                  (when-not (some #(str/includes? % block) graphic-frame-blocks)
                                    (some-> (table-shape idx block opts)
                                            (with-source opts :a/tbl idx))))
                                table-blocks))
        parsed (vec (concat parsed-shapes pics graphic-frames standalone-tables))]
    (if (seq parsed)
      parsed
      (let [texts (vec (xml-texts xml "a:t"))]
        (if (seq texts)
          (vec (map-indexed (fn [idx shape]
                              (with-source shape opts :fallback/text idx))
                            (fallback-text-shapes texts)))
          []))))))

(defn valid-shape? [shape]
  (and (map? shape)
       (contains? #{:text :rect :pic :table :chart} (:drawingml/kind shape))
       (string? (:drawingml/id shape))))
