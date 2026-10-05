(ns sdiff.html-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hiccup2.core :as hc]
            [sdiff.core :as core]
            [sdiff.render.html :as html]))

(defn- page-of [old new]
  (let [fr (core/file-report "a.clj" old new)]
    (str (hc/html (html/form-view fr (first (:forms fr)))))))

(defn- src [n] (str "(ns a)\n(defn f [x]\n" (str/join "\n" (for [i (range n)] (str "  (step-" i " x)"))) ")\n"))

(deftest a-long-form-shows-only-its-changed-region
  (let [old (src 30)
        h (page-of old (str/replace old "(step-25 x)" "(step-25 y)"))
        excerpt (second (re-find #"(?s)<div class=\"sbs excerpt\">(.*?)</div>" h))]
    (is excerpt)
    (is (str/includes? excerpt "step-23"))
    (is (not (str/includes? excerpt "step-10")))
    (is (str/includes? excerpt "⋮ 24 lines"))))

(deftest a-short-form-is-shown-whole
  (let [old (src 4)]
    (is (not (str/includes? (page-of old (str/replace old "(step-2 x)" "(step-2 y)")) "sbs excerpt")))))

(deftest sibling-changes-print-their-shared-path-once
  (let [out (with-out-str
              ((requiring-resolve 'sdiff.render.text/print-file)
               (core/file-report "a.clj"
                                 "(ns a)\n(defn f [x]\n  (let [m (g x)]\n    (h {:mode :scan :a 1} m)))\n"
                                 "(ns a)\n(defn f [x]\n  (let [m (g x)]\n    (h {:purpose :scan :a 2} m)))\n")))]
    (is (str/includes? out "› map 1 › key :mode"))
    (is (str/includes? out "‥ › key :purpose = :scan"))))
