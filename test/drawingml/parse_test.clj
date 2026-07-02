(ns drawingml.parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [drawingml.parse :as dml]))

(def layout-title-sp
  "<p:sp><p:nvSpPr><p:nvPr><p:ph type=\"title\"/></p:nvPr></p:nvSpPr>
   <p:spPr><a:xfrm><a:off x=\"457200\" y=\"274638\"/><a:ext cx=\"8229600\" cy=\"1143000\"/></a:xfrm></p:spPr>
   </p:sp>")

(def slide-title-sp-without-xfrm
  "<p:sp><p:nvSpPr><p:nvPr><p:ph type=\"title\"/></p:nvPr></p:nvSpPr>
   <p:spPr></p:spPr>
   <p:txBody><a:p><a:r><a:t>Investor update</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(deftest placeholder-geometry-inheritance-test
  (testing "layout placeholder geometry is indexed by [idx type] and [nil type]"
    (let [index (dml/placeholder-geometry-index layout-title-sp)]
      (is (= {:drawingml/x 0.5 :drawingml/y (/ 274638.0 914400)
              :drawingml/w (/ 8229600.0 914400) :drawingml/h (/ 1143000.0 914400)}
             (get index [nil "title"])))))
  (testing "a slide shape that omits <a:xfrm> inherits the layout's placeholder geometry"
    (let [index (dml/placeholder-geometry-index layout-title-sp)
          shape (dml/text-shape 0 slide-title-sp-without-xfrm {:placeholder-geometry index})]
      (is (= 0.5 (:drawingml/x shape)))
      (is (some? (:drawingml/y shape)))
      (is (not= dml/default-xfrm (select-keys shape [:drawingml/x :drawingml/y :drawingml/w :drawingml/h])))))
  (testing "with no matching layout geometry, the historical fixed fallback still applies"
    (let [shape (dml/text-shape 0 slide-title-sp-without-xfrm {})]
      (is (= (:drawingml/x dml/default-xfrm) (:drawingml/x shape))))))

(def scheme-fill-block
  "<p:sp><p:spPr><a:solidFill><a:schemeClr val=\"accent1\"/></a:solidFill></p:spPr></p:sp>")

(def multi-run-single-paragraph-sp
  "<p:sp><p:spPr></p:spPr>
   <p:txBody><a:p><a:r><a:rPr b=\"1\"/><a:t>Hello </a:t></a:r><a:r><a:t>world</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(def multi-paragraph-sp
  "<p:sp><p:spPr></p:spPr>
   <p:txBody><a:p><a:r><a:t>Line one</a:t></a:r></a:p><a:p><a:r><a:t>Line two</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(deftest paragraph-aware-text-extraction-test
  (testing "runs within one paragraph are concatenated, not split into lines"
    (is (= "Hello world" (:drawingml/text (dml/text-shape 0 multi-run-single-paragraph-sp)))))
  (testing "separate <a:p> paragraphs still become separate lines"
    (is (= "Line one\nLine two" (:drawingml/text (dml/text-shape 0 multi-paragraph-sp))))))

(def bulleted-paragraphs-sp
  "<p:sp><p:spPr></p:spPr>
   <p:txBody>
     <a:p><a:pPr algn=\"ctr\"><a:lnSpc><a:spcPct val=\"150000\"/></a:lnSpc><a:buChar char=\"&#8226;\"/></a:pPr>
       <a:r><a:t>First bullet</a:t></a:r></a:p>
     <a:p><a:pPr><a:buAutoNum type=\"arabicPeriod\"/></a:pPr>
       <a:r><a:t>Numbered item</a:t></a:r></a:p>
     <a:p><a:pPr><a:buNone/></a:pPr>
       <a:r><a:t>No bullet</a:t></a:r></a:p>
     <a:p><a:r><a:t>Default paragraph</a:t></a:r></a:p>
   </p:txBody>
   </p:sp>")

(deftest structured-paragraph-extraction-test
  (let [shape (dml/text-shape 0 bulleted-paragraphs-sp)
        paras (:drawingml/paragraphs shape)]
    (testing "flattened :drawingml/text is unaffected (still one line per paragraph)"
      (is (= "First bullet\nNumbered item\nNo bullet\nDefault paragraph" (:drawingml/text shape))))
    (testing "alignment, line-spacing, and a literal bullet character are captured"
      (is (= :center (:align (nth paras 0))))
      (is (= 1.5 (:line-spacing (nth paras 0))))
      (is (= {:type :char :char "•"} (:bullet (nth paras 0)))))
    (testing "auto-numbered bullets capture their numbering scheme"
      (is (= {:type :auto-num :scheme "arabicPeriod"} (:bullet (nth paras 1)))))
    (testing "an explicit <a:buNone/> is distinguished from \"no bullet info at all\""
      (is (= {:type :none} (:bullet (nth paras 2)))))
    (testing "a paragraph with no <a:pPr> at all carries no align/bullet/line-spacing keys"
      (is (= {:text "Default paragraph"} (nth paras 3))))))

(def table-block
  "<a:tbl>
    <a:tr><a:tc><a:txBody><a:p><a:r><a:t>Q1</a:t></a:r></a:p></a:txBody></a:tc>
          <a:tc><a:txBody><a:p><a:r><a:t>10</a:t></a:r></a:p></a:txBody></a:tc></a:tr>
    <a:tr><a:tc><a:txBody><a:p><a:r><a:t>Q2</a:t></a:r></a:p></a:txBody></a:tc>
          <a:tc><a:txBody><a:p><a:r><a:t>2</a:t></a:r><a:r><a:t>0</a:t></a:r></a:p></a:txBody></a:tc></a:tr>
   </a:tbl>")

(deftest table-rows-extraction-test
  (testing "cell grid is extracted row-major, one paragraph-aware text per cell"
    (is (= [["Q1" "10"] ["Q2" "20"]] (dml/table-rows table-block))))
  (testing "table-shape carries the grid alongside the flattened legacy text"
    (let [shape (dml/table-shape 0 table-block)]
      (is (= [["Q1" "10"] ["Q2" "20"]] (:drawingml/rows shape)))
      (is (= "Q1\n10\nQ2\n20" (:drawingml/text shape))))))

(def round-rect-themed-fill-with-text
  "A roundRect AutoShape (not :rect geometry) with a themed SHAPE fill and a
  text run that has no explicit color of its own — the shape's own
  <p:spPr><a:solidFill> must not leak into the run's text color."
  "<p:sp><p:spPr><a:prstGeom prst=\"roundRect\"/><a:solidFill><a:schemeClr val=\"accent3\"/></a:solidFill></p:spPr>
   <p:txBody><a:p><a:r><a:t>Label</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(def rect-with-explicit-run-color
  "A run WITH its own explicit color must still resolve correctly when scoped
  to txBody."
  "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/><a:solidFill><a:srgbClr val=\"EAF0F8\"/></a:solidFill></p:spPr>
   <p:txBody><a:p><a:r><a:rPr><a:solidFill><a:srgbClr val=\"112233\"/></a:solidFill></a:rPr><a:t>Label</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(deftest text-color-does-not-leak-shape-fill-test
  (testing "a non-:rect AutoShape's own theme fill is not misattributed as its text color"
    (let [shape (dml/text-shape 0 round-rect-themed-fill-with-text {:theme-colors {:accent3 "9BC15C"}})]
      (is (not= "9BC15C" (:drawingml/color shape))
          "the shape's accent3 FILL must not become the TEXT color")
      (is (= "17202A" (:drawingml/color shape))
          "no run-level color was set, so the historical text fallback applies")))
  (testing "a run's own explicit color still resolves correctly, unaffected by the shape's fill"
    (let [shape (dml/text-shape 0 rect-with-explicit-run-color {})]
      (is (= "112233" (:drawingml/color shape)))))
  (testing "the shape's own fill/geometry ARE carried on the text-shape now (just not as its text color)"
    (let [shape (dml/text-shape 0 round-rect-themed-fill-with-text {:theme-colors {:accent3 "9BC15C"}})]
      (is (= :roundRect (:drawingml/geometry shape)))
      (is (= "9BC15C" (:drawingml/fill shape)))))
  (testing "rect-shape (the text-less path) also now matches any geometry, not just exact :rect"
    (let [shape (dml/rect-shape 0 round-rect-themed-fill-with-text {:theme-colors {:accent3 "9BC15C"}})]
      (is (= :roundRect (:drawingml/geometry shape)))
      (is (= "9BC15C" (:drawingml/fill shape))))))

(def straight-connector-block
  "<p:cxnSp><p:nvCxnSpPr><p:cNvPr id=\"5\" name=\"Arrow\"/><p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr>
   <p:spPr><a:xfrm><a:off x=\"914400\" y=\"457200\"/><a:ext cx=\"1828800\" cy=\"0\"/></a:xfrm>
   <a:prstGeom prst=\"straightConnector1\"><a:avLst/></a:prstGeom>
   <a:ln><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></a:ln></p:spPr></p:cxnSp>")

(deftest connector-shape-test
  (testing "a connector, previously entirely unhandled, is now captured with its geometry/position/line color"
    (let [shape (dml/connector-shape 0 straight-connector-block)]
      (is (= :connector (:drawingml/kind shape)))
      (is (= :straightConnector1 (:drawingml/geometry shape)))
      (is (= "445566" (:drawingml/line shape)))
      (is (= 1.0 (:drawingml/x shape)))
      (is (= 0.5 (:drawingml/y shape)))
      (is (= 2.0 (:drawingml/w shape)))))
  (testing "shapes() picks up <p:cxnSp> elements alongside <p:sp>/<p:pic>"
    (let [xml (str "<p:spTree>" straight-connector-block "</p:spTree>")
          found (dml/shapes xml)]
      (is (= 1 (count found)))
      (is (= :connector (:drawingml/kind (first found)))))))

(deftest gradient-and-pattern-fill-approximate-to-a-real-color-test
  (testing "a gradient fill approximates to its first stop's color instead of falling back to nil"
    (is (= "336699"
           (dml/solid-fill
            (str "<p:sp><p:spPr><a:gradFill><a:gsLst>"
                 "<a:gs pos=\"0\"><a:srgbClr val=\"336699\"/></a:gs>"
                 "<a:gs pos=\"100000\"><a:srgbClr val=\"AABBCC\"/></a:gs>"
                 "</a:gsLst></a:gradFill></p:spPr></p:sp>")))))
  (testing "a pattern fill approximates to its foreground color"
    (is (= "992222"
           (dml/solid-fill
            (str "<p:sp><p:spPr><a:pattFill prst=\"pct20\">"
                 "<a:fgClr><a:srgbClr val=\"992222\"/></a:fgClr>"
                 "<a:bgClr><a:srgbClr val=\"FFFFFF\"/></a:bgClr>"
                 "</a:pattFill></p:spPr></p:sp>")))))
  (testing "an explicit solidFill still wins over a gradFill/pattFill in the same block"
    (is (= "112233"
           (dml/solid-fill
            (str "<p:sp><p:spPr><a:solidFill><a:srgbClr val=\"112233\"/></a:solidFill>"
                 "<a:gradFill><a:gsLst><a:gs><a:srgbClr val=\"336699\"/></a:gs></a:gsLst></a:gradFill>"
                 "</p:spPr></p:sp>"))))))

(deftest rotation-and-flip-are-read-from-xfrm-test
  (testing "rot (60,000ths of a degree) converts to plain degrees"
    (let [geom (dml/xfrm-explicit "<p:sp><p:spPr><a:xfrm rot=\"2700000\"><a:off x=\"0\" y=\"0\"/><a:ext cx=\"914400\" cy=\"914400\"/></a:xfrm></p:spPr></p:sp>")]
      (is (= 45.0 (:drawingml/rotation geom)))))
  (testing "flipH/flipV are read as booleans"
    (let [geom (dml/xfrm-explicit "<p:sp><p:spPr><a:xfrm flipH=\"1\" flipV=\"1\"><a:off x=\"0\" y=\"0\"/><a:ext cx=\"914400\" cy=\"914400\"/></a:xfrm></p:spPr></p:sp>")]
      (is (true? (:drawingml/flip-h geom)))
      (is (true? (:drawingml/flip-v geom)))))
  (testing "a shape with no rot/flip attributes carries none of those keys"
    (let [geom (dml/xfrm-explicit "<p:sp><p:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"914400\" cy=\"914400\"/></a:xfrm></p:spPr></p:sp>")]
      (is (not (contains? geom :drawingml/rotation)))
      (is (not (contains? geom :drawingml/flip-h)))
      (is (not (contains? geom :drawingml/flip-v))))))

(deftest scheme-color-resolution-test
  (testing "schemeClr resolves through the default bg/tx alias map"
    (is (= :dk1 (dml/scheme-color-role "<a:schemeClr val=\"tx1\"/>")))
    (is (= :accent1 (dml/scheme-color-role "<a:schemeClr val=\"accent1\"/>"))))
  (testing "solid-fill resolves schemeClr against a supplied theme-colors map"
    (is (= "4472C4" (dml/solid-fill scheme-fill-block {:accent1 "4472C4"}))))
  (testing "without a theme-colors map, schemeClr fills resolve to nil (caller applies its own fallback)"
    (is (nil? (dml/solid-fill scheme-fill-block nil))))
  (testing "explicit srgbClr still wins over schemeClr when both matched"
    (is (= "AABBCC" (dml/first-color "<a:srgbClr val=\"AABBCC\"/>" {:accent1 "4472C4"})))))
