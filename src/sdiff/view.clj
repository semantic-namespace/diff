(ns sdiff.view
  "A view is a lens over the decorated report: the sections a reader should see
  first, in the order and under the headings a reviewer or a model chose, with
  everything the view did not name folded at the end. Nothing is rendered that
  the full page does not have, and nothing is dropped, only deferred."
  (:require [sdiff.core :as core]
            [sdiff.decorate :as decorate]
            [sdiff.render.html :as html]))

(defn- file-of [report path] (some #(when (= path (:path %)) %) (:clj report)))

(defn- form-of [file form] (some #(when (= form (decorate/form-id %)) %) (:forms file)))

(defn- change-of [file form at]
  (some #(when (= at (core/fmt-path (:path %))) %) (:changes form)))

(defn item-problem [ctx [kind a b c :as item]]
  (case kind
    :form (decorate/validate-ref ctx {:file a :form b})
    :change (or (decorate/validate-ref ctx {:file a :form b})
                (let [f (file-of (:report ctx) a)]
                  (when-not (change-of f (form-of f b) c) (str "no change at " (pr-str c) " in " b))))
    :file (decorate/validate-ref ctx {:file a})
    :entity (decorate/validate-ref ctx {:entity a})
    :header nil
    :rename (when-not (some #(and (= a (:from %)) (= b (:to %))) (:renames (:report ctx)))
              (str "no rename " a " → " b " in this PR"))
    (str "unknown item " (pr-str item))))

(defn validate
  "`[{:section n :item i :problem s} …]` for every item that would not render."
  [ctx view]
  (vec (for [[n section] (map-indexed vector (:sections view))
             item (:items section)
             :let [p (item-problem ctx item)] :when p]
         {:section (inc n) :item item :problem p})))

(defn- render-item [report [kind a b c :as item]]
  (case kind
    :form (let [f (file-of report a) fm (when f (form-of f b))]
            (if fm
              [:div.view-item [:div.where [:code a]] (html/form-view f fm)]
              (decorate/problem "no changed form " (pr-str b) " in " a)))
    :change (let [f (file-of report a) fm (when f (form-of f b)) ch (when fm (change-of f fm c))]
              (if ch
                [:div.view-item [:div.where [:code a] " · " [:code c]]
                 (html/form-view f (assoc fm :changes [ch] :extraction nil))]
                (decorate/problem "no change at " (pr-str c) " in " (pr-str b) " of " a)))
    :header [:div.view-item (decorate/header report)]
    :rename (if-let [r (some #(when (and (= a (:from %)) (= b (:to %))) %) (:renames report))]
              [:div.view-item [:ul.renames [:li "rename " [:code a] " → " [:code b] [:span.n (str (:count r) " sites across the PR")]]]]
              (decorate/problem "no rename " a " → " b " in this PR"))
    :file (if-let [f (file-of report a)]
            (html/file-view nil nil f)
            (decorate/problem "no changed Clojure file " a))
    :entity [:div.view-item.view-entity [:h4 [:code.ent (str a)]] (decorate/entity-section a)]
    (decorate/problem "unknown item " (pr-str item))))

(defn- shown-forms [view]
  (set (for [s (:sections view) [kind a b] (:items s) :when (= :form kind)] [a b])))

(defn- shown-files [view]
  (set (for [s (:sections view) [kind a] (:items s) :when (= :file kind)] a)))

(defn- render-compact
  "A grouping as plain lists of forms, one per section, then everything the
  grouping did not place."
  [report view]
  (let [shown (shown-forms view)
        file-of* (fn [p] (file-of report p))
        rest-forms (for [f (:clj report) :when (= :semantic (:verdict f)) form (:forms f)
                         :when (not (shown [(:path f) (decorate/form-id form)]))] [f form])]
    (list
     (for [[i s] (map-indexed vector (:sections view)) :when (seq (:items s))]
       [:section.group {:id (str "view-" (inc i))}
        [:div.glabel (:title s) [:span.gcount]]
        [:div.glist (for [[kind a b] (:items s) :when (= :form kind)
                          :let [f (file-of* a) fm (when f (form-of f b))] :when fm]
                      (html/form-view f fm))]])
     (when (seq rest-forms)
       [:section.group {:id "view-rest"}
        [:div.glabel "Everything else" [:span.gcount]]
        [:div.glist (for [[f form] rest-forms] (html/form-view f form))]]))))

(declare render-full)

(defn render
  "The view's sections, then the rest of the report folded."
  [report view]
  (if (:compact view) (render-compact report view) (render-full report view)))

(defn- render-full
  [report view]
  (let [shown (shown-forms view) files (shown-files view)
        rest-files (for [f (:clj report) :when (not (files (:path f)))
                         :let [left (vec (remove #(shown [(:path f) (decorate/form-id %)]) (:forms f)))]
                         :when (or (seq left) (not= :semantic (:verdict f)))]
                     (assoc f :forms left))]
    (list
     [:header.view-head
      [:h1 (:title view)]
      (if (:derived view)
        [:p.prov "derived · " (:derived view) " · the full report, regrouped"]
        [:p.prov "inferred · " (or (:author view) "unknown") " · a view over the full report" (when (:question view) (list " · answers: " [:em (:question view)]))])
      (when (:intro view) [:p.intro (decorate/code-spans (:intro view))])]
     [:div.fold-all
      [:button {:type "button" :onclick "document.querySelectorAll('details.view-section').forEach(d=>d.open=true)"} "expand all"]
      [:button {:type "button" :onclick "document.querySelectorAll('details.view-section').forEach(d=>d.open=false)"} "collapse all"]]
     [:nav.toc (for [[i s] (map-indexed vector (:sections view))] [:a {:href (str "#view-" (inc i))} (str (inc i) ". " (:title s))])]
     (for [[i s] (map-indexed vector (:sections view))
           :let [n (count (:items s))
                 body (list (when (:claim s) [:p.claim {:class (when-not (:derived view) "inferred-claim")} (decorate/code-spans (:claim s))])
                            (for [item (:items s)] (render-item report item)))]]
       [:details.view-section {:id (str "view-" (inc i)) :open (not (:folded s))}
        [:summary [:h2 [:span.n (str (inc i))] (:title s) [:span.mute (str " · " n " item" (when (not= 1 n) "s") (when (:folded s) " · folded"))]]]
        body])
     [:details.everything-else
      [:summary (str "Everything else: " (count rest-files) " file" (when (not= 1 (count rest-files)) "s") (if (:derived view) " outside every group" " the view did not single out"))]
      (for [f rest-files] (html/file-view nil nil f))])))
