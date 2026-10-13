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

(deftest decorations-have-a-text-form
  (is (= "derived · tool\ndeclares :fn/x · changed\n- a → b\n- c\nkey v" 
         (d/hiccup->text (d/derived "tool" [:p "declares " [:code.ent ":fn/x"] " · " [:span.tag "changed"]]
                                    [:ul.ids [:li "a → b"] [:li "c"]]
                                    [:table.contract [:tbody [:tr [:td "key"] [:td "v"]]]])))))

(defn- with-declared [f]
  (let [before @d/params]
    (reset! d/params #{"against"})
    (try (f) (finally (reset! d/params before)))))

(deftest request-params-keeps-declared-non-empty-params
  (with-declared
    #(do (is (= {"against" "main"} (d/request-params {"against" "main" "other" "x"})))
         (is (= {} (d/request-params {"against" ""})))
         (is (= {} (d/request-params {}))))))

(deftest with-params-appends-declared-params-to-a-url
  (with-declared
    (fn []
      (is (= "/pr?ref=a" (d/with-params "/pr?ref=a")) "nothing bound, nothing added")
      (binding [d/*params* {"against" "deployed/dev"}]
        (is (= "/pr?ref=a&against=deployed%2Fdev" (d/with-params "/pr?ref=a")))
        (is (= "/pr?x=1" (d/with-params "/pr?x=1" {"other" "y"})) "undeclared params are dropped")
        (is (= "/x?against=a+b" (d/with-params "/x" {"against" "a b"})) "starts a query string when there is none")))))

(deftest stamped-items-are-labelled-when-their-params-differ
  (binding [d/*params* {"against" "main"}]
    (is (= {:text "x" :params {"against" "main"}} (d/stamp {:text "x"})))
    (is (nil? (d/params-note {"against" "main"})))
    (is (= "made with against=dev" (d/params-note {"against" "dev"})))
    (is (= "made without parameters" (d/params-note nil)))
    (let [h (str (hiccup2.core/html (d/render-annotation {:text "t" :author "a" :params {"against" "dev"}})))]
      (is (str/includes? h "made with against=dev"))
      (is (str/includes? h "t") "the annotation is not hidden")))
  (binding [d/*params* {}]
    (is (nil? (d/params-note nil)))
    (is (not (str/includes? (str (hiccup2.core/html (d/render-annotation {:text "t" :params {}}))) "made")))))
