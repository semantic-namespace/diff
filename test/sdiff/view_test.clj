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

(deftest a-view-made-under-other-params-is-labelled
  (let [render (fn [ps stamped] (binding [d/*ctx* {:report report :annotations []} d/*params* ps]
                                  (str (hiccup2.core/html (view/render report (assoc v :params stamped))))))]
    (is (str/includes? (render {"against" "main"} {"against" "dev"}) "made with against=dev"))
    (is (not (str/includes? (render {"against" "main"} {"against" "main"}) "made with")))
    (is (str/includes? (render {"against" "main"} {"against" "dev"}) "The extraction") "the view still renders")))

(defn- header-count [prs body]
  (d/add-header-decorator! ::marker (fn [_ _] [:p "registry-context-marker"]))
  (try
    (let [html (binding [d/*ctx* {:report report :annotations []}]
                 (if body
                   (html/page nil "t" prs :body (body))
                   (html/page nil "t" prs)))]
      (count (re-seq #"registry-context-marker" html)))
    (finally (swap! d/header-decorators #(remove (fn [[k]] (= ::marker k)) %)))))

(deftest the-header-renders-once-whatever-the-ordering
  (let [grouping {:title "risk" :compact true :derived "test"
                  :sections [{:title "Top" :items [[:form path "defn compile!"]]}]}
        with-header-item {:title "v" :sections [{:title "ctx" :items [[:header]]}
                                                {:title "x" :items [[:form path "defn compile!"]]}]}]
    (is (= 1 (header-count [report] nil)) "default report")
    (is (= 1 (header-count [report] #(binding [d/*ctx* {:report report :annotations []}] (view/render report grouping)))) "a built-in grouping")
    (is (= 1 (header-count [report] #(binding [d/*ctx* {:report report :annotations []}] (view/render report (dissoc grouping :compact))))) "a grouping rendered as a full view")
    (is (= 1 (header-count [report] #(binding [d/*ctx* {:report report :annotations []}] (view/render report with-header-item)))) "a view with a :header item")))
