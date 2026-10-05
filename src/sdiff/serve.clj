(ns sdiff.serve
  "Local review UI: the HTML report of a pull request with a review panel, served
  on 127.0.0.1 only. Notes are attached to forms and change paths in the page,
  previewed as the exact GitHub payload, and posted through `gh` as the user who
  runs the server.

  Every POST needs the token printed into the page at startup, so another site
  open in the same browser cannot post reviews through it.

  Served on any address other than loopback, for example a VPN interface, every
  request also needs an access key: the startup URL carries it once, the server
  turns it into a SameSite cookie and redirects. Without the key, anyone who can
  reach the address could read private pull requests and post reviews with the
  credentials of whoever runs the server."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as hc]
            [org.httpkit.server :as http]
            [sdiff.github :as github]
            [sdiff.render.html :as html]
            [sdiff.review :as review]))

(def token (str (random-uuid)))
(def access-key (str (random-uuid)))

(defn- resource [n] (slurp (io/resource (str "sdiff/" n))))

(defn- query-param [req k]
  (some (fn [kv] (let [[a b] (str/split kv #"=" 2)]
                   (when (= a k) (java.net.URLDecoder/decode (or b "") "UTF-8"))))
        (str/split (or (:query-string req) "") #"&")))

(defn- html-response [s] {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body s})
(defn- json-response [status m] {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string m)})

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

(defn- pr-page [ref]
  (let [r (github/cached-report ref)
        {:keys [repo num url author]} (:pr r)
        config {:ref (str repo "#" num) :repo repo :num num :url url :author author
                :head (:head r) :token token}]
    (html/page (str "https://github.com/" repo) repo [r]
               :extra-head [:style (hc/raw (resource "review.css"))]
               :extra-body (list [:script {:id "sdiff-config" :type "application/json"} (hc/raw (json/generate-string config))]
                                 [:script (hc/raw (resource "review.js"))]))))

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

(defn handler [{:keys [request-method uri] :as req}]
  (try
    (case [request-method uri]
      [:get "/"]       (html-response (index-page))
      [:get "/pr"]     (if-let [ref (query-param req "ref")]
                         (html-response (pr-page ref))
                         {:status 302 :headers {"Location" "/"}})
      [:post "/draft"] (review-call req false)
      [:post "/post"]  (review-call req true)
      {:status 404 :body "not found"})
    (catch Exception e
      {:status 500 :headers {"Content-Type" "text/plain; charset=utf-8"} :body (str "error: " (ex-message e))})))

(defn- cookie [req k]
  (some (fn [kv] (let [[a b] (str/split (str/trim kv) #"=" 2)] (when (= a k) b)))
        (str/split (or (get-in req [:headers "cookie"]) "") #";")))

(defn- keyed
  "Wraps `h` so every request needs the access key, as a cookie or once in the URL."
  [h]
  (fn [req]
    (cond
      (= access-key (cookie req "sdiff-key")) (h req)
      (= access-key (query-param req "key"))
      {:status 302
       :headers {"Location" (let [q (->> (str/split (or (:query-string req) "") #"&")
                                         (remove #(or (str/blank? %) (str/starts-with? % "key=")))
                                         (str/join "&"))]
                              (str (:uri req) (when (seq q) (str "?" q))))
                 "Set-Cookie" (str "sdiff-key=" access-key "; Path=/; HttpOnly; SameSite=Strict")}}
      :else {:status 401 :headers {"Content-Type" "text/plain; charset=utf-8"}
             :body "This sdiff server needs its access link, printed where it was started."})))

(defn loopback? [host] (contains? #{"127.0.0.1" "localhost" "::1"} host))

(defn start!
  "Serve on `host`:`port` (default 127.0.0.1); returns `{:stop f :url u}`, where
  `u` is the address to open, with the access key when the host is not loopback."
  ([port] (start! port "127.0.0.1"))
  ([port host]
   (let [local? (loopback? host)
         stop (http/run-server (if local? #'handler (keyed #'handler)) {:ip host :port port})]
     {:stop stop
      :url (str "http://" host ":" port "/" (when-not local? (str "?key=" access-key)))})))
