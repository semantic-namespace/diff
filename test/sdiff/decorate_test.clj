(ns sdiff.decorate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [sdiff.core :as c]
            [sdiff.decorate :as d]
            [sdiff.render.html :as html]))

(def path "core/src/atlas/registry.cljc")
(def file (c/file-report path (slurp (str "test/fixtures/atlas-7/old/" path)) (slurp (str "test/fixtures/atlas-7/new/" path))))
(def report {:num 7 :title "t" :clj [file] :renames []})

(deftest decorations-and-annotations-render-beside-the-form-they-target
  (d/add-form-decorator! ::t (fn [ctx f form] (when (= "defn compile!" (d/form-id form)) (d/derived "test tool" [:p (str "ctx " (:x ctx))]))))
  (d/add-header-decorator! ::t (fn [_ r] (d/derived "test tool" [:p (str (count (:clj r)) " files")])))
  (try
    (let [html (binding [d/*ctx* {:x 1 :annotations [{:on {:file path :form "defn compile!"} :author "a" :text "guess about `x`" :basis ["dependents"]}
                                                     {:on "page" :author "a" :text "overall"}]}]
                 (html/page nil "t" [report]))]
      (is (= 1 (count (re-seq #"derived · test tool</span><p>ctx 1" html))) "one form got the decoration")
      (is (str/includes? html "derived · test tool</span><p>1 files"))
      (is (= 2 (count (re-seq #"inferred · a" html))))
      (is (str/includes? html "based on <code>dependents</code>"))
      (is (str/includes? html (str "id=\"" (d/anchor file {:id ["defn" "compile!"]}) "\""))))
    (finally (swap! d/form-decorators #(remove (fn [[k]] (= ::t k)) %))
             (swap! d/header-decorators #(remove (fn [[k]] (= ::t k)) %)))))

(deftest annotation-targets-are-validated
  (let [ctx {:report report}]
    (is (nil? (d/validate-ref ctx "page")))
    (is (nil? (d/validate-ref ctx {:file path :form "defn compile!"})))
    (is (str/starts-with? (d/validate-ref ctx {:file path :form "defn nope"}) "no changed form"))
    (is (str/starts-with? (d/validate-ref ctx {:file "x.clj"}) "no changed Clojure file"))
    (is (string? (d/validate-ref ctx {:entity ":fn/x"})) "entities need a host validator")
    (d/add-ref-validator! ::t (fn [_ on] (when (:entity on) (when-not (= ":fn/x" (str (:entity on))) "no such entity"))))
    (try (is (nil? (d/validate-ref ctx {:entity ":fn/x"})))
         (is (= "no such entity" (d/validate-ref ctx {:entity ":fn/y"})))
         (finally (swap! d/ref-validators #(remove (fn [[k]] (= ::t k)) %))))))
