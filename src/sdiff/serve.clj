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
            [sdiff.deps]
            [sdiff.github :as github]
            [sdiff.group :as group]
            [sdiff.render.html :as html]
            [sdiff.render.text :as text]
            [sdiff.review :as review]
            [sdiff.decorate :as decorate]
            [sdiff.names :as names]
            [sdiff.view :as view]
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

(defn- query-params
  "The request's query string as a map of string to string."
  [req]
  (into {} (for [kv (str/split (or (:query-string req) "") #"&") :when (seq kv)
                 :let [[a b] (str/split kv #"=" 2)]]
             [(java.net.URLDecoder/decode a "UTF-8") (java.net.URLDecoder/decode (or b "") "UTF-8")])))

(defn- html-response [s] {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body s})
(defn- json-response [status m] {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string m)})

(defn- authorized [req f]
  (if (not= token (get-in req [:headers "x-sdiff-token"]))
    (json-response 403 {:error "missing or wrong token; reload the page"})
    (try (f (json/parse-string (slurp (:body req)) true))
         (catch Exception e (json-response 400 {:error (ex-message e)})))))

(declare host-routes)

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
            [:button {:type "submit"} "Open"]]
           (when-let [pages (seq (for [[[m path] {:keys [title]}] (sort-by key @host-routes) :when (= :get m)] [path title]))]
             [:ul.host-pages (for [[path title] pages] [:li [:a {:href (decorate/with-params path)} title]])])]]])))

(defn form-key [file id] (str file "|" (str/join " " (remove nil? (map str id)))))

(defn- fingerprint
  "Changes when the form's code changes, not when lines move around it."
  [{:keys [old new]} {:keys [id was changes]}]
  (let [src (fn [s k] (some-> (get (sdiff.core/index s) k) rewrite-clj.node/string (str/replace #"\s+" " ")))]
    (str (hash [(src old (or was id)) (src new id) (count changes)]))))

(defn- form-index [r]
  (merge
   (into {} (for [f (:clj r) fm (:forms f)]
              [(form-key (:path f) (:id fm))
               {:fp (fingerprint f fm) :was (when (:was fm) (form-key (:path f) (:was fm)))}]))
   (into {} (for [{:keys [path patch status]} (:other r)]
              [(str path "|file") {:fp (str (hash [status patch]))}]))))

(defn- page-config [r]
  (let [{:keys [repo num url author]} (:pr r)]
    {:ref (str repo "#" num) :repo repo :num num :url url :author author :head (:head r) :token token :forms (form-index r)
     :status (github/pr-status (:pr r))}))

(defonce ^:private names-cache (atom {}))

(defn- names-of [r]
  (let [k [(get-in r [:pr :repo]) (:num r) (:head r)]]
    (or (@names-cache k)
        (let [d (sdiff.deps/data r)
              t (names/table r (when (and d (not (:error d))) (sdiff.deps/names-analysis d r)))]
          (swap! names-cache assoc k t)
          t))))

(defn- pr-text [ref grouping params]
  (binding [decorate/*params* params]
    (let [r (github/cached-report ref)
          {:keys [repo num]} (:pr r)
          ctx (assoc (@decorate/context-fn r) :annotations (:annotations (state/review repo num)) :params params)]
      (binding [decorate/*ctx* ctx]
        (let [g (when grouping (decorate/grouping ctx grouping))
              body (str (when g (group/outline-text g)) (with-out-str (text/print-report r)))
              used (names/used (names-of r) body)]
          (str "#" num " " (:title r) "\n" (get-in r [:pr :url]) "\nrange " (subs (:base r) 0 12) ".." (subs (:head r) 0 12) "\n"
               (html/status-text (github/pr-status (:pr r))) "\n"
               (names/legend-text used) "\n"
               (names/shorten-text used body)))))))

(def ^:private orders [["risk" "risk"] ["file" "file"] ["calls" "calls"]])

(defn- switcher [ref current]
  (let [href (fn [g] (decorate/with-params (str "/pr?ref=" (java.net.URLEncoder/encode ref "UTF-8") "&group=" g)))
        known (set (map (comp name :key) @decorate/groupers))]
    [:nav.group-switch "Order "
     (for [[g label] orders :when (or (= "file" g) (known g))]
       [:a {:href (href g) :class (when (= g current) "on")} label])]))

(defn- pr-page [ref view-name grouping params]
  (let [r (github/cached-report ref)
        {:keys [repo num]} (:pr r)
        config (page-config r)
        r (assoc r :status (:status config))
        review (state/review repo num)
        ctx (binding [decorate/*params* params] (assoc (@decorate/context-fn r) :annotations (:annotations review) :params params))
        grouping (or grouping (when-not view-name (:default-order (state/settings))))
        grouping (when-not (= "file" grouping) grouping)
        v (binding [decorate/*params* params]
            (cond view-name (or (get (:views review) view-name) (get (:views review) (keyword view-name)))
                  grouping (some-> (decorate/grouping ctx grouping) (assoc :compact true))))
        grouping (if (and grouping (not view-name) (empty? (:sections v))) nil grouping)
        v (if (or view-name grouping) v nil)]
    (binding [decorate/*ctx* ctx decorate/*params* params]
      (html/page (str "https://github.com/" repo) repo [r]
                 :names (names-of r)
                 :shell true
                 :before (switcher ref (when-not view-name (or grouping "file")))
                 :body (cond v (view/render r v)
                             view-name [:p.deco.deco-problem (str "no view named " (pr-str view-name) " for this PR; views: " (pr-str (keys (:views review))))]
                             grouping [:p.deco.deco-problem (str "no grouping named " (pr-str grouping))])
                 :extra-head [:style (hc/raw (resource "review.css"))]
                 :extra-body (list [:script {:id "sdiff-config" :type "application/json"} (hc/raw (json/generate-string config))]
                                   [:script (hc/raw (resource "review.js"))])))))

(defn- body-params [params] (decorate/request-params (update-keys (or params {}) name)))

(defn- view-post [req]
  (authorized req (fn [{:keys [pr name view params]}]
                    (binding [decorate/*params* (body-params params)]
                      (let [{:keys [repo num]} (github/parse-pr pr)
                            r (github/cached-report pr)
                            v (if (string? view) (clojure.edn/read-string view) view)
                            ctx (assoc (@decorate/context-fn r) :params decorate/*params*)
                            problems (view/validate ctx v)
                            views (assoc (:views (state/review repo num)) name (decorate/stamp v))]
                        (state/save-review! repo num {:views views})
                        (json-response 200 {:url (decorate/with-params (str "/pr?ref=" (java.net.URLEncoder/encode (str repo "#" num) "UTF-8") "&view=" (java.net.URLEncoder/encode name "UTF-8")))
                                            :problems problems :views (keys views)}))))))

(defn- annotate-post [req]
  (authorized req (fn [{:keys [pr annotations params]}]
                    (binding [decorate/*params* (body-params params)]
                      (let [{:keys [repo num]} (github/parse-pr pr)
                            r (github/cached-report pr)
                            ctx (assoc (@decorate/context-fn r) :params decorate/*params*)
                            anns (vec (for [a annotations] (decorate/stamp (update a :on #(if (map? %) (update-keys % keyword) %)))))
                            problems (vec (keep (fn [a] (when-let [p (decorate/validate-ref ctx (:on a))] {:on (:on a) :problem p})) anns))]
                        (state/save-review! repo num {:annotations anns})
                        (json-response 200 {:url (decorate/with-params (str "/pr?ref=" (java.net.URLEncoder/encode (str repo "#" num) "UTF-8"))) :problems problems}))))))

(defn- review-call [req post?]
  (if (not= token (get-in req [:headers "x-sdiff-token"]))
    (json-response 403 {:error "missing or wrong token; reload the page"})
    (try
      (let [{:keys [pr verdict summary notes]} (json/parse-string (slurp (:body req)) true)
            r (github/cached-report pr)
            {:keys [payload placed]} (review/draft (assoc r :names (names-of r)) verdict summary notes)]
        (if-not post?
          (json-response 200 {:payload payload :placed placed})
          (let [{:keys [repo num]} (:pr r)
                res (json/parse-string (github/gh ["api" "-X" "POST" (str "repos/" repo "/pulls/" num "/reviews") "--input" "-"]
                                                  :in (json/generate-string payload))
                                       true)
                _ (github/forget! repo num)]
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
                      (json-response 200 (state/save-review! repo num (-> (select-keys review [:forms :notes :verdict :summary])
                                                                           (update :forms #(update-keys % (fn [k] (subs (str k) 1)))))))))))

(defn- settings-post [req]
  (authorized req (fn [{:keys [settings]}] (json-response 200 (state/save-settings! settings)))))

(def ^:private builtin-routes
  #{[:get "/"] [:get "/pr"] [:get "/viewed"] [:post "/viewed"] [:post "/annotate"] [:post "/view"]
    [:get "/state"] [:post "/state"] [:post "/settings"] [:post "/draft"] [:post "/post"]})

(defonce host-routes (atom {}))

(defn add-route!
  "Adds a page of the host's own. `f` takes the ring request, with `:params`
  holding the declared request parameters, and returns either a ring response
  (a map with `:status`) passed through as is, or hiccup, which is wrapped in
  the same shell as the other pages under `title`. `decorate/*params*` is bound
  while `f` runs. Registering a built-in route throws; registering a host
  route again replaces it. `title` also lists a GET route on the index page."
  [method path title f]
  (let [k [method path]]
    (when (builtin-routes k)
      (throw (ex-info (str "built-in route " (pr-str k) " cannot be replaced") {:route k})))
    (swap! host-routes assoc k {:title title :f f})))

(defn- host-page [title content]
  (str "<!doctype html>"
       (hc/html
        [:html {:lang "en"}
         [:head [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title]
          [:style (hc/raw (str (resource "report.css") (resource "review.css")))]]
         [:body
          [:div.wrap
           [:div.intro [:h1 title] [:p [:a {:href "/"} "Structural review"]]]
           content]]])))

(defn- host-route [{:keys [request-method uri] :as req}]
  (if-let [{:keys [title f]} (get @host-routes [request-method uri])]
    (let [params (decorate/request-params (query-params req))
          out (binding [decorate/*params* params] (f (assoc req :params params)))]
      (if (and (map? out) (:status out)) out (html-response (host-page title out))))
    {:status 404 :body "not found"}))

(defn handler [{:keys [request-method uri] :as req}]
  (try
    (case [request-method uri]
      [:get "/"]       (html-response (index-page))
      [:get "/pr"]     (if-let [ref (query-param req "ref")]
                         (let [params (decorate/request-params (query-params req))]
                           (if (= "text" (query-param req "format"))
                             {:status 200 :headers {"Content-Type" "text/plain; charset=utf-8"} :body (pr-text ref (query-param req "group") params)}
                             (html-response (pr-page ref (query-param req "view") (query-param req "group") params))))
                         {:status 302 :headers {"Location" "/"}})
      [:get "/viewed"] (try (json-response 200 (:files (github/viewed (query-param req "ref"))))
                            (catch Exception e (json-response 400 {:error (ex-message e)})))
      [:post "/viewed"] (viewed-call req)
      [:post "/annotate"] (annotate-post req)
      [:post "/view"] (view-post req)
      [:get "/state"]  (state-get req)
      [:post "/state"] (state-post req)
      [:post "/settings"] (settings-post req)
      [:post "/draft"] (review-call req false)
      [:post "/post"]  (review-call req true)
      (host-route req))
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
