(ns sdiff.blocks
  "Renderers for the blocks a review page is made of. Each takes the page
  context and a block vector and returns hiccup; a block that cannot be
  resolved renders as a visible error instead of disappearing. A host with a
  registry adds its own block kinds with `defmethod render`."
  (:require [clojure.string :as str]
            [sdiff.render.html :as html]
            [sdiff.review :as review]))

(defn- code-spans [s]
  (interpose nil (for [[i part] (map-indexed vector (str/split (str s) #"`" -1))]
                   (if (odd? i) [:code part] part))))

(defn problem [& msg] [:div.blk.blk-problem (apply str msg)])

(defmulti render (fn [_ctx [kind]] kind))

(defmethod render :default [_ [kind]] (problem "no renderer for block " (pr-str kind) " on this server"))

(defmethod render :text [_ [_ s]] [:p.blk-text (code-spans s)])

(defmethod render :diff/form [{:keys [report]} [_ file form]]
  (let [f (some #(when (= file (:path %)) %) (:clj report))
        fm (when f (#'review/find-form f form))]
    (cond (nil? f) (problem "no changed Clojure file " file)
          (nil? fm) (problem "no changed form " (pr-str form) " in " file)
          :else [:div.blk.blk-form [:div.where [:code file]] (#'html/form-view f fm)])))

(defmethod render :diff/files [{:keys [report]} _]
  [:table.blk.blk-files
   [:tbody (for [f (sort-by (comp html/verdict-order :verdict) (:clj report))]
             [:tr [:td [:span.verdict {:class (name (:verdict f))} (html/verdict-label (:verdict f))]]
              [:td [:code (:path f)]] [:td.mute (str (count (:forms f)) " form" (when (not= 1 (count (:forms f))) "s"))]])]])
