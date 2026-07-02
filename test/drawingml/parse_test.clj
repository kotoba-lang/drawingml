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
