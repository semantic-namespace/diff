(ns sdiff.serve
  "Local review UI: the HTML report of a pull request with a review panel, served
  on 127.0.0.1 only. Notes are attached to forms and change paths in the page,
  previewed as the exact GitHub payload, and posted through `gh` as the user who
  runs the server.

  Every POST needs the token printed into the page at startup, so another site
  open in the same browser cannot post reviews through it. Served on an address
  other than loopback, anyone who can reach it can read pull requests and post
  reviews as the user running the server."
  (:require [cheshire.core :as json]
            [clojure.edn]
            [rewrite-clj.node]
            [sdiff.core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as hc]
            [org.httpkit.server :as http]
            [sdiff.github :as github]
            [sdiff.render.html :as html]
            [sdiff.review :as review]
            [sdiff.doc :as doc]
            [sdiff.state :as state]))

(def token (str (random-uuid)))

(defn- announce!
  "Where this server is and its token, for local tools of the same user."
  [url]
  (let [f (io/file (state/dir) "server.edn")]
    (io/make-parents f)
    (spit f (pr-str {:url url :token token}))))

(defn- resource [n] (slurp (io/resource (str "sdiff/" n))))

(defn- query-param [req k]
  (some (fn [kv] (let [[a b] (str/split kv #"=" 2)]
                   (when (= a k) (java.net.URLDecoder/decode (or b "") "UTF-8"))))
        (str/split (or (:query-string req) "") #"&")))

(defn- html-response [s] {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body s})
(defn- json-response [status m] {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string m)})

(defn- authorized [req f]
  (if (not= token (get-in req [:headers "x-sdiff-token"]))
    (json-response 403 {:error "missing or wrong token; reload the page"})
    (try (f (json/parse-string (slurp (:body req)) true))
         (catch Exception e (json-response 400 {:error (ex-message e)})))))

(defn- index-page []
  (str "<!doctype html>"
       (hc/html
        [:html {:lang "en"}
         [:head [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title "Structural review"]
          [:style (hc/raw (str (resource "report.css") (resource "review.css")))]]
         [:body
          [:div.wrap
           [:div.intro
            [:h1 "Structural review"]
            [:p "Open a pull request you can read with your gh login. Notes and the review are posted as you."]]
           [:form.open {:action "/pr" :method "get"}
            [:input {:name "ref" :placeholder "owner/repo#123 or https://github.com/owner/repo/pull/123" :required true :autofocus true}]
            [:button {:type "submit"} "Open"]]]]])))

(defn form-key [file id] (str file "|" (str/join " " (remove nil? (map str id)))))

(defn- fingerprint
  "Changes when the form's code changes, not when lines move around it."
  [{:keys [old new]} {:keys [id was changes]}]
  (let [src (fn [s k] (some-> (get (sdiff.core/index s) k) rewrite-clj.node/string (str/replace #"\s+" " ")))]
    (str (hash [(src old (or was id)) (src new id) (count changes)]))))

(defn- form-index [r]
  (into {} (for [f (:clj r) fm (:forms f)]
             [(form-key (:path f) (:id fm))
              {:fp (fingerprint f fm) :was (when (:was fm) (form-key (:path f) (:was fm)))}])))

(defn- page-config [r]
  (let [{:keys [repo num url author]} (:pr r)]
    {:ref (str repo "#" num) :repo repo :num num :url url :author author :head (:head r) :token token :forms (form-index r)}))

(defn- pr-page [ref]
  (let [r (github/cached-report ref)
        {:keys [repo num url author]} (:pr r)
        config (page-config r)]
    (html/page (str "https://github.com/" repo) repo [r]
               :extra-head [:style (hc/raw (resource "review.css"))]
               :extra-body (list [:script {:id "sdiff-config" :type "application/json"} (hc/raw (json/generate-string config))]
                                 [:script (hc/raw (resource "review.js"))]))))

(defn- review-page [ref]
  (let [{:keys [repo num]} (github/parse-pr ref)]
    (if-let [d (state/page repo num)]
      (let [r (github/cached-report ref)]
        (doc/page d r (doc/context d r) :config (page-config r)))
      (str "<!doctype html><meta charset=utf-8><p>No review page for " repo "#" num " yet. Write one with the review-page MCP tool or POST /review-page.</p>"))))

(defn- review-page-post [req]
  (authorized req (fn [{:keys [pr document]}]
                    (let [{:keys [repo num]} (github/parse-pr pr)
                          d (if (string? document) (clojure.edn/read-string document) document)
                          r (github/cached-report pr)
                          problems (doc/validate d (doc/context d r))]
                      (state/save-page! repo num d)
                      (json-response 200 {:url (str "/review?ref=" (java.net.URLEncoder/encode (str repo "#" num) "UTF-8")) :problems problems})))))

(defn- review-call [req post?]
  (if (not= token (get-in req [:headers "x-sdiff-token"]))
    (json-response 403 {:error "missing or wrong token; reload the page"})
    (try
      (let [{:keys [pr verdict summary notes]} (json/parse-string (slurp (:body req)) true)
            r (github/cached-report pr)
            {:keys [payload placed]} (review/draft r verdict summary notes)]
        (if-not post?
          (json-response 200 {:payload payload :placed placed})
          (let [{:keys [repo num]} (:pr r)
                res (json/parse-string (github/gh ["api" "-X" "POST" (str "repos/" repo "/pulls/" num "/reviews") "--input" "-"]
                                                  :in (json/generate-string payload))
                                       true)]
            (json-response 200 {:state (:state res) :url (:html_url res) :placed placed}))))
      (catch Exception e (json-response 400 {:error (ex-message e)})))))

(defn- viewed-call [req]
  (if (not= token (get-in req [:headers "x-sdiff-token"]))
    (json-response 403 {:error "missing or wrong token; reload the page"})
    (try
      (let [{:keys [pr path viewed]} (json/parse-string (slurp (:body req)) true)
            {:keys [pr-id]} (github/viewed pr)]
        (json-response 200 {:path path :state (github/set-viewed! pr-id path (boolean viewed))}))
      (catch Exception e (json-response 400 {:error (ex-message e)})))))

(defn- state-get [req]
  (let [{:keys [repo num]} (github/parse-pr (query-param req "ref"))]
    (json-response 200 {:settings (state/settings) :review (state/review repo num)})))

(defn- state-post [req]
  (authorized req (fn [{:keys [pr review]}]
                    (let [{:keys [repo num]} (github/parse-pr pr)]
                      (json-response 200 (state/save-review! repo num (update review :forms #(update-keys % (fn [k] (subs (str k) 1))))))))))

(defn- settings-post [req]
  (authorized req (fn [{:keys [settings]}] (json-response 200 (state/save-settings! settings)))))

(defn handler [{:keys [request-method uri] :as req}]
  (try
    (case [request-method uri]
      [:get "/"]       (html-response (index-page))
      [:get "/pr"]     (if-let [ref (query-param req "ref")]
                         (html-response (pr-page ref))
                         {:status 302 :headers {"Location" "/"}})
      [:get "/viewed"] (try (json-response 200 (:files (github/viewed (query-param req "ref"))))
                            (catch Exception e (json-response 400 {:error (ex-message e)})))
      [:post "/viewed"] (viewed-call req)
      [:get "/review"] (if-let [ref (query-param req "ref")] (html-response (review-page ref)) {:status 302 :headers {"Location" "/"}})
      [:post "/review-page"] (review-page-post req)
      [:get "/state"]  (state-get req)
      [:post "/state"] (state-post req)
      [:post "/settings"] (settings-post req)
      [:post "/draft"] (review-call req false)
      [:post "/post"]  (review-call req true)
      {:status 404 :body "not found"})
    (catch Exception e
      {:status 500 :headers {"Content-Type" "text/plain; charset=utf-8"} :body (str "error: " (ex-message e))})))

(defn loopback? [host] (contains? #{"127.0.0.1" "localhost" "::1"} host))

(defn start!
  "Serve on `host`:`port` (default 127.0.0.1); returns `{:stop f :url u}`."
  ([port] (start! port "127.0.0.1"))
  ([port host]
   (let [url (str "http://" host ":" port "/")]
     (announce! url)
     {:stop (http/run-server #'handler {:ip host :port port})
      :url url})))
