(ns drawingml.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [drawingml.core :as dml]
            [drawingml.parse :as parse]))

(deftest renders-basic-drawingml
  (is (= 95250 (dml/emu 10)))
  (is (= "<a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill>"
         (dml/render (dml/solid-fill "#ff0000"))))
  (is (str/includes? (dml/render (dml/shape-properties {:x 1 :y 2 :width 10 :height 20 :fill "#fff"}))
                     "<a:xfrm>")))

(deftest renders-text-and-table
  (is (str/includes? (dml/render (dml/text-body (dml/paragraph (dml/text-run "Hello" {:size 12}))))
                     "<a:t>Hello</a:t>"))
  (is (str/includes? (dml/render (dml/table-row 10 [(dml/table-cell "A")]))
                     "<a:tr h=\"95250\">")))

(deftest validates-builder-nodes
  (is (dml/valid-node? (dml/text-body (dml/paragraph (dml/text-run "Hello")))))
  (is (not (dml/valid-node? {:tag :a:t :attrs nil :children []}))))

(deftest parses-shape-fixture
  (let [xml (str "<p:sp><p:nvSpPr><p:cNvPr id=\"2\" name=\"Headline\"/></p:nvSpPr>"
                 "<p:spPr><a:xfrm><a:off x=\"914400\" y=\"457200\"/><a:ext cx=\"2743200\" cy=\"914400\"/></a:xfrm>"
                 "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:solidFill><a:srgbClr val=\"ABCDEF\"/></a:solidFill>"
                 "<a:ln><a:solidFill><a:srgbClr val=\"112233\"/></a:solidFill></a:ln></p:spPr></p:sp>")
        parsed (first (parse/shapes xml))]
    (is (= :rect (:drawingml/kind parsed)))
    (is (= "Headline" (:drawingml/id parsed)))
    (is (= 1.0 (:drawingml/x parsed)))
    (is (= 0.5 (:drawingml/y parsed)))
    (is (= 3.0 (:drawingml/w parsed)))
    (is (= "ABCDEF" (:drawingml/fill parsed)))
    (is (= "112233" (:drawingml/line parsed)))
    (is (parse/valid-shape? parsed))))

(deftest parses-rendered-text-roundtrip
  (let [xml (str "<p:sp><p:nvSpPr><p:cNvPr name=\"Roundtrip\"/></p:nvSpPr>"
                 (dml/render (dml/shape-properties {:x 1 :y 1 :width 2 :height 1}))
                 (dml/render (dml/text-body (dml/paragraph (dml/text-run "Roundtrip" {:size 18 :color "#123456"}))))
                 "</p:sp>")
        parsed (first (parse/shapes xml))]
    (is (= "Roundtrip" (:drawingml/text parsed)))
    (is (= 18.0 (:drawingml/font-size parsed)))
    (is (= "123456" (:drawingml/color parsed)))))
