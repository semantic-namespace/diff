(ns sdiff.decorate
  "The semantic layer of the report page. A host registers decorators that
  render derived facts beside each changed form and at the top of the page,
  and a context builder that gives them what they need. Annotations are the
  inferred layer: notes a reviewer or a model attaches to a form, an entity or
  the page, each carrying its author and the derived facts it rests on. The
  two are rendered apart and labelled, so a reader always knows which is which."
  (:require [clojure.string :as str]))

(defonce context-fn (atom (fn [report] {:report report})))

(defn use-context! [f] (reset! context-fn f))

(def ^:dynamic *ctx* nil)

(defonce form-decorators (atom []))
(defonce header-decorators (atom []))
(defonce ref-validators (atom []))

(defn- put [coll k f] (conj (vec (remove #(= k (first %)) coll)) [k f]))

(defn add-form-decorator! "`(fn [ctx file form] hiccup-or-nil)`" [k f] (swap! form-decorators put k f))
(defn add-header-decorator! "`(fn [ctx report] hiccup-or-nil)`" [k f] (swap! header-decorators put k f))
(defn add-ref-validator! "`(fn [ctx on] problem-string-or-nil)`" [k f] (swap! ref-validators put k f))

(defn form-id [form] (str/join " " (remove nil? (map str (:id form)))))

(defn anchor [file form] (str "form-" (Math/abs (hash [(:path file) (form-id form)]))))

(defn code-spans [s]
  (for [[i part] (map-indexed vector (str/split (str s) #"`" -1))] (if (odd? i) [:code part] part)))

(defn derived
  "A decoration whose content came from a tool; `source` names it."
  [source & body]
  [:div.deco.derived [:span.prov "derived · " source] body])

(defn problem [& msg] [:div.deco.deco-problem (apply str msg)])

(defn- guarded [f & args]
  (try (apply f args) (catch Exception e (problem (ex-message e)))))

(defn form-decorations [file form]
  (keep (fn [[_ f]] (guarded f *ctx* file form)) @form-decorators))

(defn header [report]
  (keep (fn [[_ f]] (guarded f *ctx* report)) @header-decorators))

(defn- on= [a b] (= (if (map? a) (update-vals a str) (str a)) (if (map? b) (update-vals b str) (str b))))

(defn annotations-on [ctx on] (filter #(on= on (:on %)) (:annotations ctx)))

(defn render-annotation [{:keys [author text basis kind]}]
  [:div.deco.inferred
   [:span.prov (or kind "inferred") " · " (or author "unknown")]
   [:p (code-spans text)]
   (when (seq basis) [:p.basis "based on " (interpose ", " (for [b basis] [:code (str b)]))])])

(defn form-annotations [file form]
  (map render-annotation (annotations-on *ctx* {:file (:path file) :form (form-id form)})))

(defn validate-ref
  "Why an annotation's `:on` does not resolve, or nil when it does."
  [ctx on]
  (cond
    (= "page" (str on)) nil
    (and (map? on) (:file on))
    (let [file (some #(when (= (:file on) (:path %)) %) (get-in ctx [:report :clj]))]
      (cond (nil? file) (str "no changed Clojure file " (:file on))
            (nil? (:form on)) nil
            (not-any? #(= (:form on) (form-id %)) (:forms file)) (str "no changed form " (pr-str (:form on)) " in " (:file on))))
    (and (map? on) (:entity on))
    (if (seq @ref-validators)
      (some (fn [[_ f]] (f ctx on)) @ref-validators)
      "entity targets need a registry; this server has none")
    :else (str "unknown target " (pr-str on))))
