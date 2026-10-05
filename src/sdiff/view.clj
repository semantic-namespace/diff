(ns sdiff.view
  "A view is a lens over the decorated report: the sections a reader should see
  first, in the order and under the headings a reviewer or a model chose, with
  everything the view did not name folded at the end. Nothing is rendered that
  the full page does not have, and nothing is dropped, only deferred."
  (:require [clojure.string :as str]
            [sdiff.decorate :as decorate]
            [sdiff.render.html :as html]))

(defn- file-of [report path] (some #(when (= path (:path %)) %) (:clj report)))

(defn- form-of [file form] (some #(when (= form (decorate/form-id %)) %) (:forms file)))

(defn item-problem [ctx [kind a b :as item]]
  (case kind
    :form (decorate/validate-ref ctx {:file a :form b})
    :file (decorate/validate-ref ctx {:file a})
    :entity (decorate/validate-ref ctx {:entity a})
    (str "unknown item " (pr-str item))))

(defn validate
  "`[{:section n :item i :problem s} …]` for every item that would not render."
  [ctx view]
  (vec (for [[n section] (map-indexed vector (:sections view))
             item (:items section)
             :let [p (item-problem ctx item)] :when p]
         {:section (inc n) :item item :problem p})))

(defn- render-item [report [kind a b :as item]]
  (case kind
    :form (let [f (file-of report a) fm (when f (form-of f b))]
            (if fm
              [:div.view-item [:div.where [:code a]] (html/form-view f fm)]
              (decorate/problem "no changed form " (pr-str b) " in " a)))
    :file (if-let [f (file-of report a)]
            (html/file-view nil nil f)
            (decorate/problem "no changed Clojure file " a))
    :entity [:div.view-item.view-entity [:h4 [:code.ent (str a)]] (decorate/entity-section a)]
    (decorate/problem "unknown item " (pr-str item))))

(defn- shown-forms [view]
  (set (for [s (:sections view) [kind a b] (:items s) :when (= :form kind)] [a b])))

(defn- shown-files [view]
  (set (for [s (:sections view) [kind a] (:items s) :when (= :file kind)] a)))

(defn render
  "The view's sections, then the rest of the report folded."
  [report view]
  (let [shown (shown-forms view) files (shown-files view)
        rest-files (for [f (:clj report) :when (not (files (:path f)))
                         :let [left (vec (remove #(shown [(:path f) (decorate/form-id %)]) (:forms f)))]
                         :when (or (seq left) (not= :semantic (:verdict f)))]
                     (assoc f :forms left))]
    (list
     [:header.view-head
      [:h1 (:title view)]
      [:p.prov "inferred · " (or (:author view) "unknown") " · a view over the full report"]
      (when (:intro view) [:p.intro (decorate/code-spans (:intro view))])]
     [:nav.toc (for [[i s] (map-indexed vector (:sections view))] [:a {:href (str "#view-" (inc i))} (str (inc i) ". " (:title s))])]
     (for [[i s] (map-indexed vector (:sections view))]
       [:section.view-section {:id (str "view-" (inc i))}
        [:h2 [:span.n (str (inc i))] (:title s)]
        (when (:claim s) [:p.claim.inferred-claim (decorate/code-spans (:claim s))])
        (for [item (:items s)] (render-item report item))])
     [:details.everything-else
      [:summary (str "Everything else: " (count rest-files) " file" (when (not= 1 (count rest-files)) "s") " the view did not single out")]
      (for [f rest-files] (html/file-view nil nil f))])))
