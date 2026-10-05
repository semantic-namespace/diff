(ns sdiff.doc
  "A review page: a walk through a pull request in steps, each a claim backed
  by diff and atlas blocks. The document is data written by a reviewer or a
  model; the blocks render from the report and the registry, so every fact on
  the page is tool-derived."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as hc]
            [sdiff.blocks :as blocks]))

(defonce context-fn
  (atom (fn [_doc report] {:report report})))

(defn use-context!
  "Install the host's context builder, `(fn [doc report] ctx)`; the default
  knows only the report. A host with a registry adds what its blocks need."
  [f] (reset! context-fn f))

(defn context [doc report] (@context-fn doc report))

(defn validate
  "Blocks that would render as problems, with the reason, before any page is written."
  [doc ctx]
  (vec (for [[i step] (map-indexed vector (:steps doc))
             b (:blocks step)
             :let [h (try (blocks/render ctx b) (catch Exception e (blocks/problem (ex-message e))))]
             :when (and (vector? h) (= :div.blk.blk-problem (first h)))]
         {:step (inc i) :block b :problem (last h)})))

(defn- code-spans [s]
  (for [[i part] (map-indexed vector (str/split (str s) #"`" -1))] (if (odd? i) [:code part] part)))

(defn- resource [n] (slurp (io/resource (str "sdiff/" n))))

(defn page
  "The whole page. `config` is what the review panel script needs; nil for a
  static page without the panel."
  [doc report ctx & {:keys [config]}]
  (let [{:keys [pr num title]} report]
    (str "<!doctype html>"
         (hc/html
          [:html {:lang "en"}
           [:head [:meta {:charset "utf-8"}] [:meta {:name "viewport" :content "width=device-width, initial-scale=1, viewport-fit=cover"}]
            [:title (str "Review — #" num " " title)]
            [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
            [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap"}]
            [:style (hc/raw (str (resource "report.css") (resource "review.css") (resource "doc.css")))]]
           [:body
            [:div.wrap
             [:header.doc-head
              [:h1 [:a {:href (:url pr) :target "_blank"} (str "#" num)] " " title]
              [:p.sum (str (count (:clj report)) " Clojure files") (when-let [s (:subtitle ctx)] (list " · " s))]
              (when (:intro doc) [:p.intro (code-spans (:intro doc))])]
             [:nav.toc (for [[i s] (map-indexed vector (:steps doc))] [:a {:href (str "#step-" (inc i))} (str (inc i) ". " (:title s))])]
             (for [[i s] (map-indexed vector (:steps doc))]
               [:section.step {:id (str "step-" (inc i))}
                [:h2 [:span.n (str (inc i))] (:title s)]
                (when (:claim s) [:p.claim (code-spans (:claim s))])
                (for [b (:blocks s)]
                  (try (blocks/render ctx b) (catch Exception e (blocks/problem (ex-message e)))))])]
            (when config
              (list [:script {:id "sdiff-config" :type "application/json"} (hc/raw (json/generate-string config))]
                    [:script (hc/raw (resource "review.js"))]))]]))))
