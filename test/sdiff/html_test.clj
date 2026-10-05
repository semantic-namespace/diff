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
