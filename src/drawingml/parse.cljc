(ns drawingml.parse
  "Small DrawingML XML to EDN projection helpers."
  (:require [clojure.string :as str]))

(def emu-per-inch 914400)

(defn xml-unescape [s]
  (-> (str (or s ""))
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

(defn xfrm [block]
  (let [body (or (second (re-find #"<a:xfrm\b[^>]*>([\s\S]*?)</a:xfrm>" (or block ""))) "")
        off (or (re-find #"<a:off\b[^>]*>" body) "")
        ext (or (re-find #"<a:ext\b[^>]*>" body) "")]
    {:drawingml/x (emu->inch (xml-attr off "x") 0.8)
     :drawingml/y (emu->inch (xml-attr off "y") 0.8)
     :drawingml/w (emu->inch (xml-attr ext "cx") 8.4)
     :drawingml/h (emu->inch (xml-attr ext "cy") 0.7)}))

(defn first-color [xml]
  (some-> (or (second (re-find #"<a:srgbClr\b[^>]*\bval=\"([0-9A-Fa-f]{6})\"" (or xml "")))
              (second (re-find #"\blastClr=\"([0-9A-Fa-f]{6})\"" (or xml ""))))
          str/upper-case))

(defn solid-fill [block]
  (some-> (second (re-find #"<a:solidFill\b[^>]*>([\s\S]*?)</a:solidFill>" (or block "")))
          first-color))

(defn line-fill [block]
  (some-> (second (re-find #"<a:ln\b[^>]*>([\s\S]*?)</a:ln>" (or block "")))
          first-color))

(defn font-size [block fallback]
  (if-let [sz (some-> (re-find #"<a:rPr\b[^>]*>" (or block "")) (xml-attr "sz") parse-double-safe)]
    (double (/ sz 100))
    fallback))

(defn geometry [block]
  (some-> (re-find #"<a:prstGeom\b[^>]*>" (or block ""))
          (xml-attr "prst")
          keyword))

(defn text-shape [idx block]
  (let [texts (vec (xml-texts block "a:t"))
        text (str/join "\n" texts)]
    (when-not (str/blank? text)
      (cond-> (merge {:drawingml/id (shape-name block idx "text")
                      :drawingml/kind :text
                      :drawingml/text text
                      :drawingml/font-size (font-size block 20)
                      :drawingml/color (or (solid-fill block) "17202A")}
                     (xfrm block))
        (> (count texts) 1) (assoc :drawingml/source-kind :drawingml/text-runs)))))

(defn rect-shape [idx block]
  (when (= :rect (geometry block))
    (cond-> (merge {:drawingml/id (shape-name block idx "rect")
                    :drawingml/kind :rect
                    :drawingml/fill (or (solid-fill block) "EAF0F8")}
                   (xfrm block))
      (line-fill block) (assoc :drawingml/line (line-fill block)))))

(defn pic-shape [idx block]
  (merge {:drawingml/id (shape-name block idx "pic")
          :drawingml/kind :pic
          :drawingml/text (shape-name block idx "Picture")
          :drawingml/source-kind :drawingml/pic
          :drawingml/font-size 12
          :drawingml/color "334155"}
         (xfrm block)))

(defn table-shape [idx block]
  (let [texts (vec (xml-texts block "a:t"))]
    (when (seq texts)
      (merge {:drawingml/id (shape-name block idx "table")
              :drawingml/kind :table
              :drawingml/text (str/join "\n" texts)
              :drawingml/source-kind :drawingml/table
              :drawingml/font-size 14
              :drawingml/color "17202A"}
             (xfrm block)))))

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

(defn shapes [xml]
  (let [shape-blocks (vec (xml-elements xml "p:sp"))
        parsed-shapes (vec (keep-indexed (fn [shape-idx block]
                                           (or (text-shape shape-idx block)
                                               (rect-shape shape-idx block)))
                                         shape-blocks))
        pics (vec (map-indexed pic-shape (xml-elements xml "p:pic")))
        tables (vec (keep-indexed table-shape (xml-elements xml "a:tbl")))
        parsed (vec (concat parsed-shapes pics tables))]
    (if (seq parsed)
      parsed
      (let [texts (vec (xml-texts xml "a:t"))]
        (if (seq texts)
          (fallback-text-shapes texts)
          [])))))

(defn valid-shape? [shape]
  (and (map? shape)
       (contains? #{:text :rect :pic :table} (:drawingml/kind shape))
       (string? (:drawingml/id shape))))
