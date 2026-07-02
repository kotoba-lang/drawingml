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

(def merged-header-table-block
  "A 2-column table whose header row is one gridSpan=2 cell (with its own
  fill) followed by its hMerge continuation cell, and a plain data row
  below it."
  (str "<a:tbl>"
       "<a:tr>"
       "<a:tc gridSpan=\"2\"><a:txBody><a:p><a:r><a:t>Header</a:t></a:r></a:p></a:txBody>"
       "<a:tcPr><a:solidFill><a:srgbClr val=\"DDEEFF\"/></a:solidFill></a:tcPr></a:tc>"
       "<a:tc hMerge=\"1\"><a:txBody><a:p><a:endParaRPr/></a:p></a:txBody><a:tcPr/></a:tc>"
       "</a:tr>"
       "<a:tr>"
       "<a:tc><a:txBody><a:p><a:r><a:t>Q1</a:t></a:r></a:p></a:txBody></a:tc>"
       "<a:tc><a:txBody><a:p><a:r><a:t>10</a:t></a:r></a:p></a:txBody></a:tc>"
       "</a:tr>"
       "</a:tbl>"))

(deftest table-cell-merge-and-fill-test
  (testing "table-cells captures the anchor cell's span + fill, and the continuation as a merge marker"
    (is (= [[{:text "Header" :col-span 2 :fill "DDEEFF"} :h-merge]
            ["Q1" "10"]]
           (dml/table-cells merged-header-table-block {:accent1 "000000"}))))
  (testing "table-shape only carries :drawingml/cells when the table is non-uniform (has a merge/span/fill)"
    (let [shape (dml/table-shape 0 merged-header-table-block)]
      (is (some? (:drawingml/cells shape)))
      (is (= [{:text "Header" :col-span 2 :fill "DDEEFF"} :h-merge] (first (:drawingml/cells shape))))
      (testing ":drawingml/rows still carries the flat (legacy) text grid unaffected"
        (is (= [["Header" ""] ["Q1" "10"]] (:drawingml/rows shape))))))
  (testing "a plain uniform table (no merges/fills) gets no :drawingml/cells at all"
    (is (nil? (:drawingml/cells (dml/table-shape 0 table-block)))))
  (testing "table-non-uniform? correctly distinguishes the two cases"
    (is (true? (dml/table-non-uniform? (dml/table-cells merged-header-table-block))))
    (is (false? (dml/table-non-uniform? (dml/table-cells table-block))))))

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

(deftest apply-group-transform-test
  (testing "identity when the group's on-slide box exactly matches its child coordinate space"
    (let [group-xf {:off-x 1.0 :off-y 1.0 :ext-w 2.0 :ext-h 2.0
                    :ch-off-x 0.0 :ch-off-y 0.0 :ch-ext-w 2.0 :ch-ext-h 2.0}
          shape {:drawingml/x 0.5 :drawingml/y 0.5 :drawingml/w 1.0 :drawingml/h 1.0}]
      (is (= {:drawingml/x 1.5 :drawingml/y 1.5 :drawingml/w 1.0 :drawingml/h 1.0}
             (dml/apply-group-transform shape group-xf)))))
  (testing "a group resized to half its original child-space extent scales children down 2x"
    (let [group-xf {:off-x 0.0 :off-y 0.0 :ext-w 1.0 :ext-h 1.0
                    :ch-off-x 0.0 :ch-off-y 0.0 :ch-ext-w 2.0 :ch-ext-h 2.0}
          shape {:drawingml/x 1.0 :drawingml/y 1.0 :drawingml/w 1.0 :drawingml/h 1.0}]
      (is (= {:drawingml/x 0.5 :drawingml/y 0.5 :drawingml/w 0.5 :drawingml/h 0.5}
             (dml/apply-group-transform shape group-xf)))))
  (testing "nil group-xf (a group with no xfrm) leaves the shape untouched"
    (let [shape {:drawingml/x 1.0 :drawingml/y 1.0 :drawingml/w 1.0 :drawingml/h 1.0}]
      (is (= shape (dml/apply-group-transform shape nil))))))

(def grouped-shapes-block
  "<p:spTree>
    <p:grpSp>
      <p:grpSpPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"914400\" cy=\"914400\"/>
        <a:chOff x=\"0\" y=\"0\"/><a:chExt cx=\"1828800\" cy=\"1828800\"/></a:xfrm></p:grpSpPr>
      <p:sp><p:spPr><a:xfrm><a:off x=\"914400\" y=\"914400\"/><a:ext cx=\"914400\" cy=\"914400\"/></a:xfrm></p:spPr>
        <p:txBody><a:p><a:r><a:t>Grouped shape</a:t></a:r></a:p></p:txBody></p:sp>
    </p:grpSp>
   </p:spTree>")

(deftest grouped-shape-geometry-is-rescaled-into-slide-coordinates-test
  (let [found (dml/shapes grouped-shapes-block)
        shape (first found)]
    (testing "the group halves its child coordinate space (1in on-slide box, 2in child extent) -- child position/size scale 2x down"
      (is (= 0.5 (:drawingml/x shape)))
      (is (= 0.5 (:drawingml/y shape)))
      (is (= 0.5 (:drawingml/w shape)))
      (is (= 0.5 (:drawingml/h shape))))
    (is (= "Grouped shape" (:drawingml/text shape)))))

(def richly-formatted-run-sp
  "<p:sp><p:spPr></p:spPr>
   <p:txBody><a:p><a:r>
     <a:rPr b=\"1\" i=\"1\" u=\"sng\" strike=\"sngStrike\" baseline=\"30000\"/>
     <a:t>Fancy text</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(def plain-run-sp
  "<p:sp><p:spPr></p:spPr>
   <p:txBody><a:p><a:r><a:rPr/><a:t>Plain text</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(deftest text-run-formatting-test
  (testing "bold/italic/underline/strikethrough/baseline are all read"
    (let [shape (dml/text-shape 0 richly-formatted-run-sp)]
      (is (true? (:drawingml/bold shape)))
      (is (true? (:drawingml/italic shape)))
      (is (true? (:drawingml/underline shape)))
      (is (true? (:drawingml/strikethrough shape)))
      (is (= 30.0 (:drawingml/baseline shape)))))
  (testing "none of these keys are added when the run has no such formatting"
    (let [shape (dml/text-shape 0 plain-run-sp)]
      (is (not (contains? shape :drawingml/bold)))
      (is (not (contains? shape :drawingml/italic)))
      (is (not (contains? shape :drawingml/underline)))
      (is (not (contains? shape :drawingml/strikethrough)))
      (is (not (contains? shape :drawingml/baseline)))))
  (testing "u=\"none\"/strike=\"noStrike\" are explicit absence, not presence"
    (let [shape (dml/text-shape 0 "<p:sp><p:spPr></p:spPr><p:txBody><a:p><a:r><a:rPr u=\"none\" strike=\"noStrike\"/><a:t>x</a:t></a:r></a:p></p:txBody></p:sp>")]
      (is (not (contains? shape :drawingml/underline)))
      (is (not (contains? shape :drawingml/strikethrough))))))

(def hyperlinked-run-sp
  "<p:sp><p:spPr></p:spPr>
   <p:txBody><a:p><a:r>
     <a:rPr><a:hlinkClick r:id=\"rId3\"/></a:rPr>
     <a:t>Click here</a:t></a:r></a:p></p:txBody>
   </p:sp>")

(deftest hyperlink-resolution-test
  (testing "hlinkClick's r:id resolves through opts' :rels to the external URL"
    (let [shape (dml/text-shape 0 hyperlinked-run-sp
                                {:rels {"rId3" {:id "rId3" :target-path "https://example.com/"}}})]
      (is (= "https://example.com/" (:drawingml/hyperlink shape)))))
  (testing "no :rels entry for the rel-id -- no hyperlink key added"
    (let [shape (dml/text-shape 0 hyperlinked-run-sp {})]
      (is (not (contains? shape :drawingml/hyperlink)))))
  (testing "no hlinkClick at all -- no hyperlink key added"
    (let [shape (dml/text-shape 0 plain-run-sp {:rels {"rId3" {:target-path "https://example.com/"}}})]
      (is (not (contains? shape :drawingml/hyperlink))))))

(def dashed-line-rect-sp
  "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/>
     <a:ln><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill><a:prstDash val=\"dash\"/></a:ln>
   </p:spPr></p:sp>")

(deftest line-dash-test
  (testing "a dashed line's style is read"
    (is (= :dash (dml/line-dash dashed-line-rect-sp)))
    (is (= :dash (:drawingml/line-dash (dml/rect-shape 0 dashed-line-rect-sp)))))
  (testing "no <a:prstDash> -- solid line, no key added"
    (let [solid-sp "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/><a:ln><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></a:ln></p:spPr></p:sp>"]
      (is (nil? (dml/line-dash solid-sp)))
      (is (not (contains? (dml/rect-shape 0 solid-sp) :drawingml/line-dash))))))

(def thick-line-rect-sp
  "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/>
     <a:ln w=\"38100\"><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></a:ln>
   </p:spPr></p:sp>")

(deftest line-width-test
  (testing "a 3pt line width (38100 EMU) is read as a plain point value"
    (is (= 3.0 (dml/line-width thick-line-rect-sp)))
    (is (= 3.0 (:drawingml/line-width (dml/rect-shape 0 thick-line-rect-sp)))))
  (testing "no w attribute at all -- no key added (writer's own 1pt default applies)"
    (let [no-width-sp "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/><a:ln><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></a:ln></p:spPr></p:sp>"]
      (is (nil? (dml/line-width no-width-sp)))
      (is (not (contains? (dml/rect-shape 0 no-width-sp) :drawingml/line-width))))))

(def custom-adjusted-round-rect-sp
  "<p:sp><p:spPr><a:prstGeom prst=\"roundRect\"><a:avLst><a:gd name=\"adj\" fmla=\"val 8333\"/></a:avLst></a:prstGeom>
     <a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill>
   </p:spPr></p:sp>")

(deftest shape-adjustments-test
  (testing "a custom adjustment value (a non-default roundRect corner radius) is read verbatim"
    (is (= [{:name "adj" :fmla "val 8333"}] (dml/shape-adjustments custom-adjusted-round-rect-sp)))
    (is (= [{:name "adj" :fmla "val 8333"}] (:drawingml/adjustments (dml/rect-shape 0 custom-adjusted-round-rect-sp)))))
  (testing "an empty <a:avLst/> (the common default-adjustment case) -- no key added"
    (let [default-sp "<p:sp><p:spPr><a:prstGeom prst=\"roundRect\"><a:avLst/></a:prstGeom><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></p:spPr></p:sp>"]
      (is (nil? (dml/shape-adjustments default-sp)))
      (is (not (contains? (dml/rect-shape 0 default-sp) :drawingml/adjustments)))))
  (testing "multiple adjustment handles (a shape with 2+ adj values) all captured"
    (let [multi-adj-sp "<p:sp><p:spPr><a:prstGeom prst=\"round2SameRect\"><a:avLst><a:gd name=\"adj1\" fmla=\"val 10000\"/><a:gd name=\"adj2\" fmla=\"val 20000\"/></a:avLst></a:prstGeom><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></p:spPr></p:sp>"]
      (is (= [{:name "adj1" :fmla "val 10000"} {:name "adj2" :fmla "val 20000"}]
             (dml/shape-adjustments multi-adj-sp))))))

(def shadowed-rect-sp
  "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill>
     <a:effectLst><a:outerShdw blurRad=\"50800\" dist=\"25400\" dir=\"2700000\" rotWithShape=\"0\">
       <a:srgbClr val=\"000000\"><a:alpha val=\"40000\"/></a:srgbClr>
     </a:outerShdw></a:effectLst>
   </p:spPr></p:sp>")

(deftest shape-shadow-test
  (testing "an outer shadow's blur/distance/angle/color/alpha are all read"
    (let [shadow (dml/shape-shadow shadowed-rect-sp nil)]
      (is (= 4.0 (:blur shadow)))
      (is (= 2.0 (:distance shadow)))
      (is (= 45.0 (:angle shadow)))
      (is (= "000000" (:color shadow)))
      (is (= 40.0 (:alpha shadow)))))
  (testing "wired into rect-shape as :drawingml/shadow"
    (is (some? (:drawingml/shadow (dml/rect-shape 0 shadowed-rect-sp)))))
  (testing "no <a:effectLst> at all -- no shadow, no key added"
    (let [plain-sp "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></p:spPr></p:sp>"]
      (is (nil? (dml/shape-shadow plain-sp nil)))
      (is (not (contains? (dml/rect-shape 0 plain-sp) :drawingml/shadow))))))

(def picture-filled-rect-sp
  "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/>
     <a:blipFill><a:blip r:embed=\"rId5\"/><a:stretch><a:fillRect/></a:stretch></a:blipFill>
   </p:spPr></p:sp>")

(deftest picture-fill-as-shape-fill-test
  (testing "a shape's own fill IS a picture (<a:blipFill> in <p:spPr>, distinct from <p:pic>) -- rel-id and resolved part are captured"
    (is (= "rId5" (dml/blip-fill-rel-id picture-filled-rect-sp)))
    (is (= "ppt/media/image3.png"
           (dml/blip-fill-part picture-filled-rect-sp {:rels {"rId5" {:target-path "ppt/media/image3.png"}}})))
    (let [shape (dml/rect-shape 0 picture-filled-rect-sp {:rels {"rId5" {:target-path "ppt/media/image3.png"}}})]
      (is (= "rId5" (:drawingml/fill-image-rel-id shape)))
      (is (= "ppt/media/image3.png" (:drawingml/fill-image-part shape)))))
  (testing "a normal solidFill shape has no blip-fill keys at all"
    (let [solid-sp "<p:sp><p:spPr><a:prstGeom prst=\"rect\"/><a:solidFill><a:srgbClr val=\"445566\"/></a:solidFill></p:spPr></p:sp>"]
      (is (nil? (dml/blip-fill-rel-id solid-sp)))
      (is (not (contains? (dml/rect-shape 0 solid-sp) :drawingml/fill-image-rel-id)))))
  (testing "a text-shape variant also carries the blip-fill reference"
    (let [text-block "<p:sp><p:spPr><a:blipFill><a:blip r:embed=\"rId7\"/></a:blipFill></p:spPr><p:txBody><a:p><a:r><a:t>Label</a:t></a:r></a:p></p:txBody></p:sp>"]
      (is (= "rId7" (:drawingml/fill-image-rel-id (dml/text-shape 0 text-block)))))))

(deftest first-color-prefers-pre-resolved-clr-map-alias-test
  (testing "when theme-colors already has the RAW alias key (a custom clrMap resolution), it wins over the default bg/tx translation"
    (is (= "AABBCC"
           (dml/first-color "<a:schemeClr val=\"tx1\"/>" {:tx1 "AABBCC" :dk1 "112233"}))))
  (testing "without a pre-resolved alias key, falls back to the default bg/tx->dk/lt translation"
    (is (= "112233"
           (dml/first-color "<a:schemeClr val=\"tx1\"/>" {:dk1 "112233"})))))

(def nested-grouped-shapes-block
  "A group inside a group: the outer group halves its 4in child space into a
  2in on-slide box; the INNER group (itself living in the outer's child
  coordinate space) also halves its 2in child space into a 1in box; the
  shape sits at (1in,1in)/1in in the inner group's own child space. Both
  transform levels must compose for the shape to land correctly on the
  slide -- expected: inner maps (1,1)/1x1 -> (1.5,1.5)/0.5x0.5 (in the
  outer's child space), then outer maps that -> (0.75,0.75)/0.25x0.25 (slide
  coordinates)."
  "<p:spTree>
    <p:grpSp>
      <p:grpSpPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"1828800\" cy=\"1828800\"/>
        <a:chOff x=\"0\" y=\"0\"/><a:chExt cx=\"3657600\" cy=\"3657600\"/></a:xfrm></p:grpSpPr>
      <p:grpSp>
        <p:grpSpPr><a:xfrm><a:off x=\"914400\" y=\"914400\"/><a:ext cx=\"914400\" cy=\"914400\"/>
          <a:chOff x=\"0\" y=\"0\"/><a:chExt cx=\"1828800\" cy=\"1828800\"/></a:xfrm></p:grpSpPr>
        <p:sp><p:spPr><a:xfrm><a:off x=\"914400\" y=\"914400\"/><a:ext cx=\"914400\" cy=\"914400\"/></a:xfrm></p:spPr>
          <p:txBody><a:p><a:r><a:t>Nested grouped shape</a:t></a:r></a:p></p:txBody></p:sp>
      </p:grpSp>
    </p:grpSp>
   </p:spTree>")

(deftest nested-group-transform-composition-test
  (let [found (dml/shapes nested-grouped-shapes-block)
        shape (first found)]
    (testing "both the inner AND outer group transforms are composed, not just the immediate parent's"
      (is (= 0.75 (:drawingml/x shape)))
      (is (= 0.75 (:drawingml/y shape)))
      (is (= 0.25 (:drawingml/w shape)))
      (is (= 0.25 (:drawingml/h shape))))
    (testing "the shape's :drawingml/group still names its IMMEDIATE (innermost) parent, unchanged from the single-level case"
      (is (some? (:drawingml/group shape))))
    (testing "the outer group's own block is no longer truncated at the inner group's close -- both blocks are extracted with correct, non-overlapping-wrong bounds"
      (is (= 2 (count (#'dml/nested-group-blocks nested-grouped-shapes-block)))))))

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
