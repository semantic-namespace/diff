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
(defonce entity-renderers (atom []))

(defn- put [coll k f] (conj (vec (remove #(= k (first %)) coll)) [k f]))

(defn add-form-decorator! "`(fn [ctx file form] hiccup-or-nil)`" [k f] (swap! form-decorators put k f))
(defn add-header-decorator! "`(fn [ctx report] hiccup-or-nil)`" [k f] (swap! header-decorators put k f))
(defn add-ref-validator! "`(fn [ctx on] problem-string-or-nil)`" [k f] (swap! ref-validators put k f))
(defn add-entity-renderer! "`(fn [ctx id] hiccup-or-nil)`, the derived view of one entity for a view item" [k f] (swap! entity-renderers put k f))

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

(defn entity-section [id]
  (let [parts (keep (fn [[_ f]] (guarded f *ctx* id)) @entity-renderers)]
    (if (seq parts)
      (list parts (map render-annotation (annotations-on *ctx* {:entity id})))
      (problem "no renderer for entities on this server"))))

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

(def ^:private block-tags #{:div :p :h4 :h5 :li :tr :ul :table :tbody :details :summary})

(defn hiccup->text
  "The text a decoration carries, one line per block element, for a model or a
  terminal to read what the page shows."
  [h]
  (letfn [(tag-of [x] (keyword (first (str/split (name (first x)) #"[.#]"))))
          (walk [x]
            (cond (string? x) x
                  (nil? x) ""
                  (vector? x) (if (keyword? (first x))
                                (let [tag (tag-of x)
                                      kids (remove map? (rest x))
                                      inner (apply str (map walk kids))
                                      inner (if (= :td tag) (str inner " ") inner)]
                                  (if (block-tags tag) (str "\n" (if (= :li tag) "- " "") inner) inner))
                                (apply str (map walk x)))
                  (seq? x) (apply str (map walk x))
                  :else (str x)))]
    (->> (str/split-lines (walk h))
         (map str/trimr)
         (remove str/blank?)
         (str/join "\n"))))

(defn form-text [file form]
  (concat (map hiccup->text (form-decorations file form))
          (map hiccup->text (form-annotations file form))))

(defn header-text [report]
  (concat (map hiccup->text (header report))
          (map hiccup->text (map render-annotation (annotations-on *ctx* "page")))))
