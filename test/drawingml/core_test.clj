(ns drawingml.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [drawingml.core :as dml]))

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
