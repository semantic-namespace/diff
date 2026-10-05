(ns sdiff.doc-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [sdiff.core :as c]
            [sdiff.doc :as doc]
            [sdiff.blocks :as blocks]))

(def path "core/src/atlas/registry.cljc")
(def report {:num 7 :title "t" :pr {:url "https://example/pull/7"}
             :clj [(c/file-report path (slurp (str "test/fixtures/atlas-7/old/" path)) (slurp (str "test/fixtures/atlas-7/new/" path)))]})

(def document
  {:intro "An `intro`."
   :steps [{:title "Files" :blocks [[:diff/files]]}
           {:title "The extraction" :claim "`compile!` delegates."
            :blocks [[:diff/form path "defn compile!"] [:text "ok"]
                     [:diff/form path "defn nope"] [:atlas/contract :fn/x]]}]})

(deftest validate-names-every-block-that-cannot-render
  (is (= [{:step 2 :block [:diff/form path "defn nope"] :problem (str "no changed form \"defn nope\" in " path)}
          {:step 2 :block [:atlas/contract :fn/x] :problem "no renderer for block :atlas/contract on this server"}]
         (doc/validate document (doc/context document report)))))

(deftest page-renders-steps-and-problems
  (let [html (doc/page document report (doc/context document report))]
    (is (str/includes? html "id=\"step-2\""))
    (is (str/includes? html "data-form=\"defn compile!\""))
    (is (= 2 (count (re-seq #"class=\"blk blk-problem\"" html))))
    (is (not (str/includes? html "sdiff-config")) "no panel without a config")))

(deftest a-host-can-add-block-kinds
  (defmethod blocks/render :test/hello [_ [_ who]] [:p.blk (str "hello " who)])
  (is (empty? (doc/validate {:steps [{:blocks [[:test/hello "x"]]}]} {})))
  (remove-method blocks/render :test/hello))
