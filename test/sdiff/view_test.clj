(ns sdiff.view-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [sdiff.core :as c]
            [sdiff.decorate :as d]
            [sdiff.render.html :as html]
            [sdiff.view :as view]))

(def path "core/src/atlas/registry.cljc")
(def file (c/file-report path (slurp (str "test/fixtures/atlas-7/old/" path)) (slurp (str "test/fixtures/atlas-7/new/" path))))
(def other (c/file-report "other.clj" "(defn a [] 1)" "(defn a [] 2)"))
(def report {:num 7 :title "t" :clj [file other] :renames []})

(def v {:title "Safe to merge?" :author "a" :intro "two things"
        :sections [{:title "The extraction" :claim "`compile!` delegates" :items [[:form path "defn compile!"] [:form path "defn dev-id-conflicts"]]}
                   {:title "Elsewhere" :items [[:entity :fn/x] [:form path "defn nope"]]}]})

(deftest a-view-orders-what-it-names-and-folds-the-rest
  (let [html (binding [d/*ctx* {:report report :annotations []}] (str (hiccup2.core/html (view/render report v))))
        pos #(str/index-of html %)]
    (is (< (pos "defn compile!") (pos "defn dev-id-conflicts")) "sections keep the view's order")
    (is (str/includes? html "inferred · a · a view over the full report"))
    (is (str/includes? html "Everything else: 2 files"))
    (is (< (pos "Everything else") (pos "data-form=\"defn a\"")) "the unnamed file is in the folded rest")
    (is (= 1 (count (re-seq #"data-form=\"defn compile!\"" html))) "a named form is not repeated in the rest")
    (is (str/includes? html "no changed form &quot;defn nope&quot;"))
    (is (str/includes? html "no renderer for entities"))))

(deftest view-items-are-validated
  (is (= [{:section 2 :item [:entity :fn/x] :problem "entity targets need a registry; this server has none"}
          {:section 2 :item [:form path "defn nope"] :problem (str "no changed form \"defn nope\" in " path)}]
         (view/validate {:report report} v))))

(deftest the-other-item-kinds
  (let [v2 {:title "t" :question "is it safe?"
            :sections [{:title "one change" :items [[:change path "defn compile!" "arity 1 › let 2 › binding dev-id-conflicts"]]}
                       {:title "ctx" :folded true :items [[:header] [:rename "a" "b"]]}]}
        rep (assoc report :renames [{:from "a" :to "b" :count 2}])
        html (binding [d/*ctx* {:report rep :annotations []}] (str (hiccup2.core/html (view/render rep v2))))]
    (is (str/includes? html "answers: <em>is it safe?</em>"))
    (is (= 1 (count (re-seq #"class=\"chg chg-sem\"" (subs html 0 (str/index-of html "Everything else"))))) "a :change item shows only that change; the form's other changes stay in the rest")
    (is (str/includes? html "<details class=\"view-section\"") "a folded section is a closed details")
    (is (str/includes? html "2 sites across the PR"))
    (is (empty? (view/validate {:report rep} v2)))
    (is (= "no change at \"nope\" in defn compile!" (:problem (first (view/validate {:report rep} {:sections [{:items [[:change path "defn compile!" "nope"]]}]})))))
    (is (str/starts-with? (:problem (first (view/validate {:report rep} {:sections [{:items [[:rename "x" "y"]]}]}))) "no rename"))))
