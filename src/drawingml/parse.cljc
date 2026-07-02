(ns drawingml.parse
  "Small DrawingML XML to EDN projection helpers."
  (:require [clojure.string :as str]
            [xml.parse :as xp]))

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

(defn- balanced-tag-spans
  "Nesting-aware [start end] index pairs for every open-tag...close-tag
  region in xml, INCLUDING tags nested within themselves (e.g. a group
  inside a group) -- unlike xml-elements' non-greedy regex, which for a
  self-nesting tag matches only up to the FIRST closing tag found,
  truncating an outer element at its innermost child's close and losing the
  outer's true extent (and anything after that inner close). open-tag/
  close-tag must be exact literal strings (no attributes on open-tag) --
  true for <p:grpSp>, the one tag in this package that legitimately nests."
  [xml open-tag close-tag]
  (let [open-len (count open-tag)
        close-len (count close-tag)]
    (loop [pos 0 stack [] spans []]
      (let [open-at (str/index-of xml open-tag pos)
            close-at (str/index-of xml close-tag pos)]
        (cond
          (nil? close-at) spans
          (and open-at (< open-at close-at))
          (recur (+ open-at open-len) (conj stack open-at) spans)
          :else
          (if (seq stack)
            (let [start (peek stack)
                  stack' (pop stack)]
              (recur (+ close-at close-len) stack' (conj spans [start (+ close-at close-len)])))
            (recur (+ close-at close-len) stack spans)))))))

(defn- nested-group-blocks
  "Every <p:grpSp>...</p:grpSp> block in xml, any nesting depth, each
  correctly bounded by its OWN matching close tag. Ordered innermost-closes-
  first (a group's closing tag is found before its parent's)."
  [xml]
  (mapv (fn [[start end]] (subs xml start end))
        (balanced-tag-spans (or xml "") "<p:grpSp>" "</p:grpSp>")))

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

(defn- containing-groups
  "Every group block that contains `block` (excluding block itself, when
  block IS a group), innermost (smallest span) first -- the shape's full
  ancestor chain when it's nested inside a group inside a group, not just
  its immediate parent."
  [groups block]
  (->> groups
       (filter (fn [g] (and (not= g block) (str/includes? g block))))
       (sort-by count)))

(defn- group-metadata [groups block]
  (when-let [group-block (first (containing-groups groups block))]
    (let [idx (first (keep-indexed (fn [i g] (when (= g group-block) i)) groups))]
      {:index idx
       :id (shape-name group-block idx "group")})))

(defn- add-group [shape group]
  (cond-> shape
    group (assoc :drawingml/group group)))

(defn- group-xfrm
  "A <p:grpSp>'s own transform: off/ext (its box in SLIDE coordinates) and
  chOff/chExt (the origin/extent of the coordinate space its CHILDREN's own
  xfrm off/ext are expressed in -- almost never identical to off/ext, since
  PowerPoint lets a group's child coordinate space be scaled independently
  of the group's on-slide box, e.g. when the group has been resized after
  grouping). nil when the group has no xfrm at all."
  [group-block]
  (when-let [body (second (re-find #"<a:xfrm\b[^>]*>([\s\S]*?)</a:xfrm>" (or group-block "")))]
    (let [off (or (re-find #"<a:off\b[^>]*>" body) "")
          ext (or (re-find #"<a:ext\b[^>]*>" body) "")
          ch-off (or (re-find #"<a:chOff\b[^>]*>" body) "")
          ch-ext (or (re-find #"<a:chExt\b[^>]*>" body) "")]
      {:off-x (emu->inch (xml-attr off "x") 0)
       :off-y (emu->inch (xml-attr off "y") 0)
       :ext-w (emu->inch (xml-attr ext "cx") 1)
       :ext-h (emu->inch (xml-attr ext "cy") 1)
       :ch-off-x (emu->inch (xml-attr ch-off "x") 0)
       :ch-off-y (emu->inch (xml-attr ch-off "y") 0)
       :ch-ext-w (emu->inch (xml-attr ch-ext "cx") 1)
       :ch-ext-h (emu->inch (xml-attr ch-ext "cy") 1)})))

(defn apply-group-transform
  "Maps a shape's xfrm from its group's CHILD coordinate space into slide
  coordinates: absolute = group-off + (child-pos - group-chOff) * (group-ext
  / group-chExt). A shape whose own :drawingml/x/y/w/h were read straight off
  its <a:xfrm> (correct only when the shape is NOT inside a resized group)
  is silently wrong once a group has been resized after its children were
  grouped -- chOff/chExt no longer match off/ext 1:1, and every child's
  coordinates need this rescale to land in the right place on the slide."
  [shape group-xf]
  (if-let [{:keys [off-x off-y ext-w ext-h ch-off-x ch-off-y ch-ext-w ch-ext-h]} group-xf]
    (let [scale-x (if (zero? ch-ext-w) 1.0 (/ ext-w ch-ext-w))
          scale-y (if (zero? ch-ext-h) 1.0 (/ ext-h ch-ext-h))]
      (cond-> shape
        (:drawingml/x shape) (update :drawingml/x #(+ off-x (* scale-x (- % ch-off-x))))
        (:drawingml/y shape) (update :drawingml/y #(+ off-y (* scale-y (- % ch-off-y))))
        (:drawingml/w shape) (update :drawingml/w #(* scale-x %))
        (:drawingml/h shape) (update :drawingml/h #(* scale-y %))))
    shape))

(defn- apply-group-geometry
  "Rescales a shape's geometry into slide coordinates, composing through
  EVERY group it's nested inside (innermost first, then that group's own
  parent, and so on) -- a shape inside a group inside a group needs both
  transform levels applied in order, not just the immediate parent's."
  [shape groups block]
  (reduce (fn [shp group-block] (apply-group-transform shp (group-xfrm group-block)))
          shape
          (containing-groups groups block)))

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
  expects the slide layout/master geometry to apply instead. Rotation (`rot`,
  60,000ths of a degree) and flip (`flipH`/`flipV`) live as attributes on the
  xfrm tag itself, alongside off/ext -- previously read nowhere, so a rotated
  or flipped shape silently imported upright/unflipped."
  [block]
  (when-let [[_ open-tag body] (or (re-find #"(<a:xfrm\b[^>]*>)([\s\S]*?)</a:xfrm>" (or block ""))
                                   (re-find #"(<p:xfrm\b[^>]*>)([\s\S]*?)</p:xfrm>" (or block "")))]
    (let [off (or (re-find #"<a:off\b[^>]*>" body) "")
          ext (or (re-find #"<a:ext\b[^>]*>" body) "")
          rotation (some-> (xml-attr open-tag "rot") parse-double-safe (/ 60000.0))]
      (cond-> {:drawingml/x (emu->inch (xml-attr off "x") 0.8)
               :drawingml/y (emu->inch (xml-attr off "y") 0.8)
               :drawingml/w (emu->inch (xml-attr ext "cx") 8.4)
               :drawingml/h (emu->inch (xml-attr ext "cy") 0.7)}
        rotation (assoc :drawingml/rotation rotation)
        (= "1" (xml-attr open-tag "flipH")) (assoc :drawingml/flip-h true)
        (= "1" (xml-attr open-tag "flipV")) (assoc :drawingml/flip-v true)))))

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

;; The OOXML default colour map (used when no explicit <p:clrMap> override is
;; available to the caller): bg1/tx1/bg2/tx2 are the placeholder-facing
;; aliases PowerPoint actually emits in <a:schemeClr val="...">, resolved
;; here to the theme's dk/lt slots.
(def ^:private default-color-map
  {:bg1 :lt1 :tx1 :dk1 :bg2 :lt2 :tx2 :dk2})

(defn- raw-scheme-color-role
  "The <a:schemeClr val=\"...\"/> value as-is (bg1/tx1/accent1/...), with no
  bg/tx-alias translation applied."
  [xml]
  (some-> (second (re-find #"<a:schemeClr\b[^>]*\bval=\"([A-Za-z0-9]+)\"" (or xml "")))
          str/lower-case
          keyword))

(defn scheme-color-role
  "bg1/tx1/bg2/tx2 translated to their default (dk/lt) theme slot; every
  other role (accent1-6, hlink, folHlink) passes through unchanged. This is
  the OOXML *default* clrMap -- a deck with a non-default <p:clrMap>
  override should resolve through that instead (see first-color's
  theme-colors lookup, which checks for a pre-resolved alias entry first)."
  [xml]
  (let [role (raw-scheme-color-role xml)]
    (get default-color-map role role)))

(defn first-color
  "theme-colors may contain the alias keys (:bg1/:tx1/:bg2/:tx2) directly,
  pre-resolved through the SLIDE'S OWN (possibly non-default) <p:clrMap> by
  the caller (see presentationml.parse/theme-color-map-for-slide) -- checked
  before falling back to the OOXML default bg/tx->dk/lt translation, so a
  deck with a custom clrMap resolves schemeClr correctly instead of always
  assuming the default mapping."
  ([xml] (first-color xml nil))
  ([xml theme-colors]
   (or (some-> (or (second (re-find #"<a:srgbClr\b[^>]*\bval=\"([0-9A-Fa-f]{6})\"" (or xml "")))
                   (second (re-find #"\blastClr=\"([0-9A-Fa-f]{6})\"" (or xml ""))))
               str/upper-case)
       (let [role (raw-scheme-color-role xml)]
         (or (get theme-colors role)
             (get theme-colors (get default-color-map role role)))))))

(defn- fill-block
  "The content of a shape's fill element, whichever kind it is. A gradient
  fill approximates to its FIRST stop's color and a pattern fill to its
  foreground color -- not faithful, but a real (if imperfect) color beats
  silently collapsing to the hardcoded shape-fallback color, which is what
  happened when only <a:solidFill> was recognized."
  [block]
  (or (second (re-find #"<a:solidFill\b[^>]*>([\s\S]*?)</a:solidFill>" (or block "")))
      (second (re-find #"<a:gradFill\b[^>]*>([\s\S]*?)</a:gradFill>" (or block "")))
      (second (re-find #"<a:pattFill\b[^>]*>([\s\S]*?)</a:pattFill>" (or block "")))))

(defn- spPr-block [block]
  (second (re-find #"<p:spPr\b[^>]*>([\s\S]*?)</p:spPr>" (or block ""))))

(defn blip-fill-rel-id
  "A shape's own fill, when it's a picture used AS the fill (<a:blipFill>
  inside <p:spPr> -- distinct from <p:pic>, where the picture IS the whole
  shape). The blip's r:embed relationship id, or nil when the shape's own
  fill isn't a picture at all. Previously totally unhandled: fill-block only
  recognizes solidFill/gradFill/pattFill, so a blipFill shape's fill was
  silently dropped, degrading to the hardcoded default fill color with no
  trace it had ever been picture-filled."
  [block]
  (some->> (spPr-block block)
           (re-find #"<a:blipFill\b[^>]*>[\s\S]*?<a:blip\b[^>]*\br:embed=\"([^\"]*)\"")
           second))

(defn blip-fill-part
  "The blip fill's relationship resolved to a package part path via opts'
  :rels (the same slide-relationship map chart-shape/hyperlink-url already
  use)."
  [block opts]
  (when-let [rel-id (blip-fill-rel-id block)]
    (get-in opts [:rels rel-id :target-path])))

(defn solid-fill
  ([block] (solid-fill block nil))
  ([block theme-colors]
   (some-> (fill-block block) (first-color theme-colors))))

(defn line-fill
  ([block] (line-fill block nil))
  ([block theme-colors]
   (some-> (second (re-find #"<a:ln\b[^>]*>([\s\S]*?)</a:ln>" (or block "")))
           (first-color theme-colors))))

(defn line-width
  "A shape/connector line's width in points, from <a:ln w=\"...\"> (EMU,
  12700 per point). nil when the line has no explicit width attribute at
  all (the writer's own default, 1pt, applied when this is missing).
  Previously unread anywhere -- every written line always exported at a
  hardcoded 1pt regardless of the source deck's actual line weight."
  [block]
  (some-> (re-find #"<a:ln\b[^>]*>" (or block ""))
          (xml-attr "w")
          parse-double-safe
          (/ 12700.0)))

(defn line-dash
  "A shape/connector line's dash style (:dash, :dashDot, :lgDash, :sysDot,
  ...) from <a:ln>...<a:prstDash val=\"...\"/>..., or nil for the default
  solid line. Previously unread anywhere -- every line always exported
  solid regardless of the source deck's actual dash pattern."
  [block]
  (some-> (second (re-find #"<a:ln\b[^>]*>([\s\S]*?)</a:ln>" (or block "")))
          (->> (re-find #"<a:prstDash\b[^>]*\bval=\"([A-Za-z]+)\""))
          second
          keyword))

(defn shape-shadow
  "A shape's own outer shadow (<p:spPr>'s <a:effectLst><a:outerShdw
  blurRad=\"...\" dist=\"...\" dir=\"...\">...color...</a:outerShdw>
  </a:effectLst>), as {:blur pt :distance pt :angle deg :color hex :alpha
  pct}, or nil when the shape has no shadow. Only outerShdw -- the single
  most common real-deck effect -- is read; other effect types (glow,
  reflection, blur, soft edge, 3D bevel) remain entirely unhandled, a
  documented rather than silent limitation. Previously effects were
  unread anywhere in this package at all."
  [block theme-colors]
  (let [effect-lst (some-> (spPr-block block)
                           (->> (re-find #"<a:effectLst\b[^>]*>([\s\S]*?)</a:effectLst>"))
                           second)
        outer-open (some->> effect-lst (re-find #"<a:outerShdw\b[^>]*>"))
        outer-body (some-> effect-lst
                           (->> (re-find #"<a:outerShdw\b[^>]*>([\s\S]*?)</a:outerShdw>"))
                           second)]
    (when outer-open
      (let [color (first-color outer-body theme-colors)
            alpha (some-> (re-find #"<a:alpha\b[^>]*\bval=\"(\d+)\"" (or outer-body "")) second
                          parse-double-safe (/ 1000.0))]
        (cond-> {:blur (some-> (xml-attr outer-open "blurRad") parse-double-safe (/ 12700.0))
                 :distance (some-> (xml-attr outer-open "dist") parse-double-safe (/ 12700.0))
                 :angle (some-> (xml-attr outer-open "dir") parse-double-safe (/ 60000.0))}
          color (assoc :color color)
          alpha (assoc :alpha alpha))))))

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

(defn- first-rpr [block]
  (re-find #"<a:rPr\b[^>]*>" (or block "")))

(defn bold? [block]
  (= "1" (xml-attr (first-rpr block) "b")))

(defn italic? [block]
  (= "1" (xml-attr (first-rpr block) "i")))

(defn underline? [block]
  (let [u (xml-attr (first-rpr block) "u")]
    (boolean (and u (not= "none" u)))))

(defn strikethrough? [block]
  (let [s (xml-attr (first-rpr block) "strike")]
    (boolean (and s (not= "noStrike" s)))))

(defn baseline
  "The run's baseline shift as a plain percentage (30.0 = 30% superscript,
  -25.0 = 25% subscript), converted from OOXML's raw thousandths-of-a-
  percent `baseline` attribute. nil when absent (no superscript/subscript)."
  [block]
  (some-> (xml-attr (first-rpr block) "baseline") parse-double-safe (/ 1000.0)))

(defn- first-rpr-block
  "The first run's full <a:rPr>...</a:rPr> (or self-closing <a:rPr/>), for
  finding CHILD elements (like <a:hlinkClick>) that first-rpr's opening-tag-
  only match can't see."
  [block]
  (or (re-find #"<a:rPr\b[^>]*>[\s\S]*?</a:rPr>" (or block ""))
      (re-find #"<a:rPr\b[^>]*/>" (or block ""))))

(defn hyperlink-rel-id [block]
  (second (re-find #"<a:hlinkClick\b[^>]*\br:id=\"([^\"]*)\"" (or (first-rpr-block block) ""))))

(defn hyperlink-url
  "The run's hyperlink target URL, resolved through opts' :rels (the same
  slide-relationship map chart-shape already uses for chart-rel-id) --
  resolve-target passes an external URL (TargetMode=\"External\", the normal
  case for a hyperlink) through unchanged, so target-path already holds it."
  [block opts]
  (when-let [rel-id (hyperlink-rel-id block)]
    (get-in (:rels opts) [rel-id :target-path])))

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

(defn- table-cell-attrs [cell-block]
  (let [tag (or (re-find #"<a:tc\b[^>]*>" (or cell-block "")) "")]
    {:col-span (some-> (xml-attr tag "gridSpan") parse-double-safe long)
     :row-span (some-> (xml-attr tag "rowSpan") parse-double-safe long)
     :h-merge? (= "1" (xml-attr tag "hMerge"))
     :v-merge? (= "1" (xml-attr tag "vMerge"))}))

(defn- table-cell-fill [cell-block theme-colors]
  (some-> (second (re-find #"<a:tcPr\b[^>]*>([\s\S]*?)</a:tcPr>" (or cell-block "")))
          (solid-fill theme-colors)))

(defn table-cells
  "The table's cell grid, one entry per <a:tc> in document order, each
  either:
  - a plain string (the common case: no merge, no per-cell fill)
  - {:text ... :col-span N :row-span N :fill \"hex\"} for the ANCHOR cell
    of a merge and/or a cell with its own background fill
  - :h-merge/:v-merge/:hv-merge for a grid position covered by a preceding
    cell's merge (OOXML still emits a <a:tc> there, hMerge=\"1\"/
    vMerge=\"1\"/both, but it carries no content of its own).
  Previously table-rows collapsed EVERY cell to its flattened paragraph
  text regardless of merge/style -- a merged header row (a very common
  real-deck pattern) silently duplicated its text into cells that should
  have been empty merge continuations, and any per-cell background color
  was dropped entirely."
  ([block] (table-cells block nil))
  ([block theme-colors]
   (vec (for [row (xml-elements block "a:tr")]
          (vec (for [cell (xml-elements row "a:tc")]
                 (let [{:keys [col-span row-span h-merge? v-merge?]} (table-cell-attrs cell)]
                   (cond
                     (and h-merge? v-merge?) :hv-merge
                     h-merge? :h-merge
                     v-merge? :v-merge
                     :else
                     (let [text (paragraphs-text cell)
                           fill (table-cell-fill cell theme-colors)
                           span? (or (and col-span (> col-span 1)) (and row-span (> row-span 1)))]
                       (if (or span? fill)
                         (cond-> {:text text}
                           (and col-span (> col-span 1)) (assoc :col-span col-span)
                           (and row-span (> row-span 1)) (assoc :row-span row-span)
                           fill (assoc :fill fill))
                         text))))))))))

(defn table-non-uniform?
  "True when table-cells' grid has any merge/span/per-cell-fill cell -- the
  cue a caller (table-shape) uses to decide whether the richer :cells grid
  is worth carrying alongside the always-present flat :rows text grid."
  [cells]
  (boolean (some (fn [row] (some #(or (map? %) (keyword? %)) row)) cells)))

(defn geometry [block]
  (some-> (re-find #"<a:prstGeom\b[^>]*>" (or block ""))
          (xml-attr "prst")
          keyword))

(defn- path-command-from-node
  "One drawing command node (:a/moveTo/:a/lnTo/:a/cubicBezTo/:a/quadBezTo/
  :a/arcTo/:a/close, as parsed by xml.parse) into the same {:cmd ...} shape
  this package has always produced. moveTo/lnTo/cubicBezTo/quadBezTo carry
  their <a:pt> points verbatim; arcTo carries its own wR/hR/stAng/swAng
  attributes; close carries nothing."
  [node]
  (let [cmd-name (name (xp/el-tag node))]
    (case cmd-name
      "close" {:cmd :close}
      "arcTo" {:cmd :arcTo
               :w-radius (some-> (xp/el-attr node "wR") parse-double-safe)
               :h-radius (some-> (xp/el-attr node "hR") parse-double-safe)
               :start-angle (some-> (xp/el-attr node "stAng") parse-double-safe)
               :swing-angle (some-> (xp/el-attr node "swAng") parse-double-safe)}
      {:cmd (keyword cmd-name)
       :pts (vec (for [pt (xp/el-elements node)
                       :when (= "pt" (name (xp/el-tag pt)))]
                   {:x (some-> (xp/el-attr pt "x") parse-double-safe)
                    :y (some-> (xp/el-attr pt "y") parse-double-safe)}))})))

(defn custom-geometry
  "A shape's own <a:custGeom> path data (mutually exclusive with
  <a:prstGeom> in OOXML -- a shape has one or the other), as a vector of
  {:width N :height N :fill-rule \"...\" :commands [...]} maps (one per
  <a:path> -- a compound geometry, e.g. a shape with a hole, can have more
  than one). Raw coordinates stay in the path's own local unit space (NOT
  EMU/inches) and are preserved verbatim rather than reinterpreted --
  faithfully round-tripping the exact source path beats attempting to
  render/re-derive it, which this package has no vector rasterizer for
  anyway. nil when the shape has no <a:custGeom> at all (the overwhelming
  common case -- prstGeom presets). Previously entirely unhandled: a
  custom-path shape had NO geometry captured at all and, since rect-shape's
  own gate required a recognized preset, was silently dropped on import.

  Parses the isolated <a:custGeom> substring through xml.parse instead of
  regex-scanning its command structure directly -- the old regex needed a
  single alternation pattern to handle moveTo/lnTo/cubicBezTo/quadBezTo
  (paired tags with <a:pt> children) and arcTo/close (self-closing, plain
  attributes) uniformly; walking the parsed tree needs no such special
  casing at all."
  [block]
  (when-let [cust-xml (re-find #"<a:custGeom\b[^>]*>[\s\S]*?</a:custGeom>" (or block ""))]
    (let [cust-tree (xp/parse cust-xml)
          paths (xp/find-all cust-tree :a/path)]
      (not-empty
       (vec (for [path paths]
              (cond-> {:width (some-> (xp/el-attr path "w") parse-double-safe)
                       :height (some-> (xp/el-attr path "h") parse-double-safe)
                       :commands (mapv path-command-from-node (xp/el-elements path))}
                (xp/el-attr path "fill") (assoc :fill-rule (xp/el-attr path "fill")))))))))

(defn shape-adjustments
  "A shape's own <a:prstGeom>'s adjustment handle values (<a:avLst><a:gd
  name=\"...\" fmla=\"...\"/>...</a:avLst>), as a vector of {:name ... :fmla
  ...} maps (raw fmla string preserved verbatim rather than interpreted --
  formulas vary in structure by shape type, and round-tripping the exact
  source text is always faithful where reinterpreting it might not be), or
  nil for an empty/absent <a:avLst> (the common case for most shapes).
  Previously unread anywhere -- a shape with a customized adjustment (e.g.
  a roundRect with a non-default corner radius, a custom arrowhead ratio)
  always round-tripped to the geometry's DEFAULT adjustment instead of the
  source's actual one."
  [block]
  (when-let [geom-xml (re-find #"<a:prstGeom\b[^>]*>[\s\S]*?</a:prstGeom>" (or block ""))]
    (let [gds (xp/find-all (xp/parse geom-xml) :a/gd)]
      (not-empty
       (vec (for [gd gds
                  :let [gd-name (xp/el-attr gd "name") fmla (xp/el-attr gd "fmla")]
                  :when (and gd-name fmla)]
              {:name gd-name :fmla fmla}))))))

(defn text-shape
  "A shape with a text label. When it also has a non-default geometry
  (roundRect, oval, ...) and/or its own fill/line, those are carried too
  (:drawingml/geometry/:drawingml/fill/:drawingml/line) instead of being
  silently dropped -- previously only :rect-geometry, TEXT-LESS shapes
  (rect-shape) preserved fill/line at all, so any styled AutoShape that also
  had a label (a very common combination -- a callout box, a colored
  process-diagram step) lost its box entirely on export, leaving only bare
  text floating at the same position."
  ([idx block] (text-shape idx block {}))
  ([idx block opts]
   (let [texts (vec (xml-texts block "a:t"))
         text (paragraphs-text block)
         paras (paragraphs block)
         geom (geometry block)
         fill (solid-fill block (:theme-colors opts))
         line (line-fill block (:theme-colors opts))]
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
         (seq paras) (assoc :drawingml/paragraphs paras)
         (and geom (not= :rect geom)) (assoc :drawingml/geometry geom)
         fill (assoc :drawingml/fill fill)
         line (assoc :drawingml/line line)
         (bold? block) (assoc :drawingml/bold true)
         (italic? block) (assoc :drawingml/italic true)
         (underline? block) (assoc :drawingml/underline true)
         (strikethrough? block) (assoc :drawingml/strikethrough true)
         (baseline block) (assoc :drawingml/baseline (baseline block))
         (hyperlink-url block opts) (assoc :drawingml/hyperlink (hyperlink-url block opts))
         (line-dash block) (assoc :drawingml/line-dash (line-dash block))
         (line-width block) (assoc :drawingml/line-width (line-width block))
         (blip-fill-rel-id block) (assoc :drawingml/fill-image-rel-id (blip-fill-rel-id block))
         (blip-fill-part block opts) (assoc :drawingml/fill-image-part (blip-fill-part block opts))
         (shape-adjustments block) (assoc :drawingml/adjustments (shape-adjustments block))
         (shape-shadow block (:theme-colors opts)) (assoc :drawingml/shadow (shape-shadow block (:theme-colors opts)))
         (custom-geometry block) (assoc :drawingml/custom-geometry (custom-geometry block)))))))

(defn rect-shape
  "A styled AutoShape with NO text label. Matches any recognized
  <a:prstGeom> (rect, roundRect, ellipse, arrows, stars, ...) OR a custom-
  path <a:custGeom> (:drawingml/geometry :custom, its raw path data in
  :drawingml/custom-geometry) -- previously a custGeom shape (mutually
  exclusive with prstGeom, so this gate alone required a recognized preset)
  was silently dropped entirely on import. The actual preset is carried as
  :drawingml/geometry so a writer can reproduce the real outline instead of
  always drawing a plain rectangle."
  ([idx block] (rect-shape idx block {}))
  ([idx block opts]
   (let [geom (geometry block)
         custom (custom-geometry block)]
     (when (or geom custom)
       (cond-> (add-placeholder
                (merge {:drawingml/id (shape-name block idx "rect")
                        :drawingml/kind :rect
                        :drawingml/geometry (or geom :custom)
                        :drawingml/fill (or (solid-fill block (:theme-colors opts)) "EAF0F8")}
                       (xfrm block opts))
                block)
         (line-fill block (:theme-colors opts)) (assoc :drawingml/line (line-fill block (:theme-colors opts)))
         (line-dash block) (assoc :drawingml/line-dash (line-dash block))
         (line-width block) (assoc :drawingml/line-width (line-width block))
         (blip-fill-rel-id block) (assoc :drawingml/fill-image-rel-id (blip-fill-rel-id block))
         (blip-fill-part block opts) (assoc :drawingml/fill-image-part (blip-fill-part block opts))
         (shape-adjustments block) (assoc :drawingml/adjustments (shape-adjustments block))
         (shape-shadow block (:theme-colors opts)) (assoc :drawingml/shadow (shape-shadow block (:theme-colors opts)))
         custom (assoc :drawingml/custom-geometry custom))))))

(defn pic-blip-rel-id
  "A <p:pic>'s own image, <a:blipFill><a:blip r:embed=\"...\"/>. Previously
  unread anywhere -- a picture shape only ever carried its position/size,
  with zero reference back to which actual image part it was, even though
  the reference itself (not the raw bytes) is cheap to capture and
  sufficient for an update-path patch to preserve or re-target it."
  [block]
  (some-> (re-find #"<a:blip\b[^>]*\br:embed=\"([^\"]*)\"" (or block "")) second))

(defn pic-video-rel-id
  "A <p:pic>'s linked video, <p:nvPr><a:videoFile r:link=\"...\"/>.
  Previously unread anywhere -- video media was entirely unhandled."
  [block]
  (some-> (re-find #"<a:videoFile\b[^>]*\br:link=\"([^\"]*)\"" (or block "")) second))

(defn pic-audio-rel-id
  "A <p:pic>'s linked audio, <p:nvPr><a:audioFile r:link=\"...\"/>.
  Previously unread anywhere -- audio media was entirely unhandled."
  [block]
  (some-> (re-find #"<a:audioFile\b[^>]*\br:link=\"([^\"]*)\"" (or block "")) second))

(defn pic-shape
  ([idx block] (pic-shape idx block {}))
  ([idx block opts]
   (let [blip-rel (pic-blip-rel-id block)
         video-rel (pic-video-rel-id block)
         audio-rel (pic-audio-rel-id block)]
     (cond-> (merge {:drawingml/id (shape-name block idx "pic")
                     :drawingml/kind :pic
                     :drawingml/text (shape-name block idx "Picture")
                     :drawingml/source-kind :drawingml/pic
                     :drawingml/font-size 12
                     :drawingml/color "334155"}
                    (xfrm block opts))
       blip-rel (assoc :drawingml/image-rel-id blip-rel)
       (:target-path (get (:rels opts) blip-rel)) (assoc :drawingml/image-part (:target-path (get (:rels opts) blip-rel)))
       video-rel (assoc :drawingml/video-rel-id video-rel)
       (:target-path (get (:rels opts) video-rel)) (assoc :drawingml/video-part (:target-path (get (:rels opts) video-rel)))
       audio-rel (assoc :drawingml/audio-rel-id audio-rel)
       (:target-path (get (:rels opts) audio-rel)) (assoc :drawingml/audio-part (:target-path (get (:rels opts) audio-rel)))))))

(defn table-shape
  ([idx block] (table-shape idx block {}))
  ([idx block opts]
   (let [texts (vec (xml-texts block "a:t"))
         rows (table-rows block)
         cells (table-cells block (:theme-colors opts))]
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
         (seq rows) (assoc :drawingml/rows rows)
         (table-non-uniform? cells) (assoc :drawingml/cells cells))))))

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

(defn connector-shape
  "A <p:cxnSp> connector line/arrow between shapes. Previously totally
  unhandled (zero references anywhere in this package) -- connectors are
  common in flowcharts/diagrams and were silently vanishing on import."
  ([idx block] (connector-shape idx block {}))
  ([idx block opts]
   (cond-> (merge {:drawingml/id (shape-name block idx "connector")
                   :drawingml/kind :connector
                   :drawingml/geometry (or (geometry block) :straightConnector1)
                   :drawingml/line (or (line-fill block (:theme-colors opts)) "334155")}
                  (xfrm block opts))
     (line-dash block) (assoc :drawingml/line-dash (line-dash block))
     (line-width block) (assoc :drawingml/line-width (line-width block))
     (shape-adjustments block) (assoc :drawingml/adjustments (shape-adjustments block)))))

(defn shapes
  ([xml] (shapes xml {}))
  ([xml opts]
  (let [groups (nested-group-blocks xml)
        shape-blocks (vec (xml-elements xml "p:sp"))
        graphic-frame-blocks (vec (xml-elements xml "p:graphicFrame"))
        table-blocks (vec (xml-elements xml "a:tbl"))
        parsed-shapes (vec (keep-indexed (fn [shape-idx block]
                                           (some-> (or (text-shape shape-idx block opts)
                                                       (rect-shape shape-idx block opts))
                                                   (add-group (group-metadata groups block))
                                                   (apply-group-geometry groups block)
                                                   (with-source opts :p/sp shape-idx)))
                                         shape-blocks))
        pics (vec (map-indexed (fn [idx block]
                                  (-> (pic-shape idx block opts)
                                      (add-group (group-metadata groups block))
                                      (apply-group-geometry groups block)
                                      (with-source opts :p/pic idx)))
                                (xml-elements xml "p:pic")))
        connectors (vec (map-indexed (fn [idx block]
                                        (-> (connector-shape idx block opts)
                                            (add-group (group-metadata groups block))
                                            (apply-group-geometry groups block)
                                            (with-source opts :p/cxnSp idx)))
                                      (xml-elements xml "p:cxnSp")))
        graphic-frames (vec (keep-indexed (fn [idx block]
                                             (some-> (graphic-frame-shape idx block opts)
                                                     (add-group (group-metadata groups block))
                                                     (apply-group-geometry groups block)
                                                     (with-source opts :p/graphicFrame idx)))
                                           graphic-frame-blocks))
        standalone-tables (vec (keep-indexed
                                (fn [idx block]
                                  (when-not (some #(str/includes? % block) graphic-frame-blocks)
                                    (some-> (table-shape idx block opts)
                                            (with-source opts :a/tbl idx))))
                                table-blocks))
        parsed (vec (concat parsed-shapes pics connectors graphic-frames standalone-tables))]
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
       (contains? #{:text :rect :pic :table :chart :connector} (:drawingml/kind shape))
       (string? (:drawingml/id shape))))
