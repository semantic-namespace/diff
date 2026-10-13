(ns sdiff.serve-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [cheshire.core :as json]
            [sdiff.core :as c]
            [sdiff.decorate :as d]
            [sdiff.github :as github]
            [sdiff.serve :as serve]
            [sdiff.state :as state]))

(def report
  {:num 7 :title "t" :base (apply str (repeat 40 "a")) :head (apply str (repeat 40 "b"))
   :pr {:repo "o/r" :num 7 :url "https://github.com/o/r/pull/7" :title "t"}
   :clj [(c/file-report "a.clj" "(defn a [] 1)" "(defn a [] 2)")] :renames [] :other []})

(defn- tmp-dir [] (str (java.nio.file.Files/createTempDirectory "sdiff-serve" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- serving [f]
  (let [seen (atom [])
        before [@d/params @d/context-fn]]
    (reset! d/params #{"against"})
    (d/use-context! (fn [r] (swap! seen conj d/*params*) {:report r}))
    (try
      (binding [state/*dir* (tmp-dir)]
        (with-redefs [github/cached-report (constantly report)
                      github/pr-status (constantly {:state "open"})
                      serve/names-of (constantly {})]
          (f seen)))
      (finally (reset! d/params (first before)) (reset! d/context-fn (second before))))))

(defn- post [uri body]
  (serve/handler {:request-method :post :uri uri :headers {"x-sdiff-token" serve/token}
                  :body (java.io.ByteArrayInputStream. (.getBytes (json/generate-string body)))}))

(deftest the-context-fn-sees-the-requests-declared-params
  (serving
   (fn [seen]
     (serve/handler {:request-method :get :uri "/pr" :query-string "ref=o%2Fr%237&format=text&against=main&other=x"})
     (is (= {"against" "main"} (last @seen)))
     (serve/handler {:request-method :get :uri "/pr" :query-string "ref=o%2Fr%237"})
     (is (= {} (last @seen))))))

(deftest the-page-keeps-params-in-its-order-links
  (serving
   (fn [_]
     (let [body (:body (serve/handler {:request-method :get :uri "/pr" :query-string "ref=o%2Fr%237&against=deployed%2Fdev&other=x"}))]
       (is (str/includes? body "against=deployed%2Fdev"))
       (is (not (str/includes? body "other=x")))))))

(deftest saved-annotations-and-views-carry-their-params
  (serving
   (fn [seen]
     (let [a (json/parse-string (:body (post "/annotate" {:pr "o/r#7" :params {:against "main" :other "x"} :annotations [{:on "page" :text "t"}]})) true)
           v (json/parse-string (:body (post "/view" {:pr "o/r#7" :name "v" :params {"against" "main"} :view {:title "s" :sections []}})) true)
           review (state/review "o/r" 7)]
       (is (= {"against" "main"} (last @seen)))
       (is (str/ends-with? (:url a) "&against=main"))
       (is (str/ends-with? (:url v) "&against=main"))
       (is (= {"against" "main"} (:params (first (:annotations review)))))
       (is (= {"against" "main"} (:params (get-in review [:views "v"]))))
       (let [body (:body (serve/handler {:request-method :get :uri "/pr" :query-string "ref=o%2Fr%237&against=dev"}))]
         (is (str/includes? body "made with against=main")))))))
