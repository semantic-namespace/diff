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

(deftest a-long-form-shows-its-changed-lines-and-elides-the-rest
  (let [old (src 30)
        h (page-of old (str/replace old "(step-25 x)" "(step-25 y)"))]
    (is (str/includes? h "class=\"panes\""))
    (is (re-find #"ln elided[^>]*>.*?⋮ 24 lines" h) "the unchanged start is folded into one row")
    (is (re-find #"class=\"ln ln-del\"|<mark class=\"del\">" h))
    (is (str/includes? h "<span class=\"gut\">27</span>") "head lines carry their line numbers")))

(deftest a-short-form-is-shown-whole
  (let [old (src 4)]
    (is (not (str/includes? (page-of old (str/replace old "(step-2 x)" "(step-2 y)")) "ln elided")))))

(deftest base-and-head-rows-line-up
  (let [old "(ns a)\n(defn f [x]\n  (a x)\n  (b x))\n"
        new "(ns a)\n(defn f [x]\n  (a x)\n  (new-step x)\n  (b x))\n"
        h (page-of old new)
        rows (fn [pane] (count (re-seq #"class=\"ln" pane)))
        [base head] (rest (str/split h #"class=\"pane\""))]
    (is (= (rows base) (rows head)) "a spacer stands in for the inserted line")
    (is (not (str/includes? head "ln spacer")) "the head side has every line")
    (is (str/includes? base "ln spacer"))))

(deftest sibling-changes-print-their-shared-path-once
  (let [out (with-out-str
              ((requiring-resolve 'sdiff.render.text/print-file)
               (core/file-report "a.clj"
                                 "(ns a)\n(defn f [x]\n  (let [m (g x)]\n    (h {:mode :scan :a 1} m)))\n"
                                 "(ns a)\n(defn f [x]\n  (let [m (g x)]\n    (h {:purpose :scan :a 2} m)))\n")))]
    (is (str/includes? out "› map 1 › key :mode"))
    (is (str/includes? out "‥ › key :purpose = :scan"))))

(deftest source-blocks-carry-syntax-classes-around-the-change-marks
  (let [h (page-of "(ns a)\n(defn f [x] (g x :k \"s\")) ; c\n" "(ns a)\n(defn f [x] (g x :k \"t\")) ; c\n")]
    (is (str/includes? h "<span class=\"t-special\">defn</span>"))
    (is (str/includes? h "<span class=\"t-head\">g</span>"))
    (is (str/includes? h "<span class=\"t-kw\">:k</span>"))
    (is (str/includes? h "<mark class=\"add\"><span class=\"t-str\">&quot;t&quot;</span></mark>") "a changed string is marked and coloured")))

(deftest a-line-changed-in-place-faces-its-counterpart
  (let [old "(ns a)\n(defn f [x]\n  (a x)\n  (b x))\n"
        h (page-of old (str/replace old "(a x)" "(a y)"))]
    (is (not (str/includes? h "ln spacer")))))

(deftest a-form-is-summed-up-in-a-few-words
  (let [fr (core/file-report "a.clj" "(ns a)\n(defn f [x] (g x))\n" "(ns a)\n(defn f [x y] (g x))\n(defn h [] 1)\n")
        by-name (into {} (map (juxt #(second (:id %)) identity)) (:forms fr))]
    (is (= "new function" (html/summary (by-name "h"))))
    (is (re-find #"^adds y" (html/summary (by-name "f"))))))

(deftest a-removed-form-reads-red-and-a-new-one-green
  (let [fr (core/file-report "a.clj" "(ns a)\n(defn gone [x]\n  (inc x))\n" "(ns a)\n(defn fresh [x]\n  (dec x))\n")
        view (fn [status] (str (hc/html (html/form-view fr (first (filter #(= status (some-> % :changes first :op)) (:forms fr)))))))
        lines (fn [h cls] (count (re-seq (re-pattern (str "class=\"ln " cls "\"")) h)))]
    (is (= 2 (lines (view :removed-form) "ln-del")))
    (is (= 2 (lines (view :added-form) "ln-add")))))

(deftest a-line-only-base-has-reads-removed
  (let [h (page-of "(ns a)\n(defn f [x]\n  (a x)\n  (gone x)\n  (b x))\n" "(ns a)\n(defn f [x]\n  (a x)\n  (b x))\n")
        [base] (rest (str/split h #"class=\"pane\""))]
    (is (re-find #"class=\"ln ln-del\"><span class=\"gut\">4</span>" base))))

(deftest a-data-form-is-named-by-the-path-its-changes-share
  (let [fr (core/file-report "deps.edn"
                             "{:paths [\"src\"]\n :aliases {:test {:extra-paths [\"test\"] :main-opts [\"-m\" \"x\"]}}}\n"
                             "{:paths [\"src\"]\n :aliases {:test {:extra-paths [\"test\" \"t2\"] :main-opts [\"-m\" \"y\"]}}}\n")
        h (str (hc/html (html/form-view fr (first (:forms fr)))))]
    (is (str/includes? h "data-label=\":aliases › :test\""))
    (is (not (str/includes? (page-of "(ns a)\n(defn f [x] (a x))\n" "(ns a)\n(defn f [x] (b x))\n") "data-label")) "a call keeps its own name")))
