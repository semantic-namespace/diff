(ns sdiff.group-test
  (:require [clojure.test :refer [deftest is]]
            [sdiff.core :as core]
            [sdiff.decorate :as decorate]
            [sdiff.group :as group]))

(def report
  {:head "h"
   :clj [(core/file-report "src/a.clj"
                           "(ns a (:require [b :as b]))\n(defn f [x] (b/g x))\n(defn lone [] 1)\n"
                           "(ns a (:require [b :as b]))\n(defn f [x] (b/g (inc x)))\n(defn lone [] 2)\n")
         (core/file-report "src/b.clj"
                           "(ns b)\n(defn g [x] x)\n"
                           "(ns b)\n(defn g [x] [x])\n")]})

(deftest forms-that-call-one-another-share-a-group
  (let [[g singles] (group/by-calls {} report)]
    (is (= #{[:form "src/a.clj" "defn f"] [:form "src/b.clj" "defn g"]} (set (:items g))))
    (is (= [[:form "src/a.clj" "defn lone"]] (:items singles)))))

(deftest a-grouping-is-a-derived-view
  (let [v (decorate/grouping {:report report} "directory")]
    (is (= "sdiff · paths" (:derived v)))
    (is (= ["src"] (map :title (:sections v))))))

(deftest components-are-largest-first
  (is (= [[:a :b :c] [:d]] (group/components [:a :b :c :d] #{#{:a :b} #{:b :c}}))))
