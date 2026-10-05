(ns sdiff.review
  "Notes written against form paths become a GitHub pull-request review.

  A note is `{:file :form :at :body}`: `:form` names a top-level form as the
  report prints it (`defn compile!`, or just `compile!` when that is unique in
  the file), `:at` optionally names a change inside it by its printed path
  (`arity 1 › let 2 › binding dev-id-conflicts`), or by any end of that path
  no other change in the form shares (`‥ › binding dev-id-conflicts`). Files,
  forms and paths may be written with the short names a report's `:names`
  table gives them. `:line` (and `:start-line`) may be given instead, as an
  escape hatch.

  Each note is anchored to head-side lines: a change's new expression, or for a
  form-level note the first line of the form that the PR touched. GitHub only
  accepts inline comments inside the PR's diff hunks, so a note whose lines are
  all outside them, or that sits on a removed form, is placed in the review
  body instead. Nothing is dropped; the result says where each note went."
  (:require [clojure.string :as str]
            [sdiff.core :as core]
            [sdiff.hunks :as hunks]
            [sdiff.names :as names]))

(def events {"approve" "APPROVE" "request-changes" "REQUEST_CHANGES" "comment" "COMMENT"})

(defn- id-str [id] (str/join " " (remove nil? (map str id))))

(def ^:private ^:dynamic *names* nil)

(defn- spells? [given full] (or (= given full) (and *names* (= given (names/shorten-text *names* full)))))

(defn- find-form [{:keys [forms]} form]
  (let [form (str/trim (str form))]
    (or (some #(when (spells? form (id-str (:id %))) %) forms)
        (let [hits (filter #(some (partial spells? form) (map str (:id %))) forms)]
          (when (= 1 (count hits)) (first hits))))))

(defn- elided [at] (str/replace (str/trim at) #"^(‥|…|\.\.)\s*›\s*" ""))

(defn find-change
  "The change in `form` that `at` names: its whole printed path, or an end of
  it that no other change in the form shares."
  [{:keys [changes]} at]
  (let [at (elided at)
        path-of (comp core/fmt-path :path)
        ends? (fn [c] (let [p (path-of c)]
                        (or (spells? at p)
                            (some #(spells? at (core/fmt-path (drop % (:path c)))) (range 1 (count (:path c)))))))]
    (or (some #(when (spells? at (path-of %)) %) changes)
        (let [hits (filter ends? changes)] (when (= 1 (count hits)) (first hits))))))

(defn- rows [node] (let [{:keys [row end-row]} (meta node)] (when row [row end-row])))

(defn- head-node [{:keys [new]} {:keys [id]}] (get (core/index new) id))

(defn locate
  "Head-side `[from to]` lines a note should sit on, or `{:error reason}`."
  [file note]
  (if (:line note)
    [(or (:start-line note) (:line note)) (:line note)]
    (let [f (find-form file (:form note))]
      (cond
        (nil? f) {:error (str "no changed form named " (pr-str (:form note)) " in " (:path file))}
        (= :removed-form (:op (first (:changes f)))) {:error "the form was removed, so it has no line in the new code"}
        (:at note)
        (let [c (find-change f (:at note))]
          (cond (nil? c) {:error (str "no change at " (pr-str (:at note)) " in " (id-str (:id f)))}
                (:new c) (rows (:new c))
                :else (rows (head-node file f))))
        :else
        (let [[s e] (rows (head-node file f))
              [hs] (hunks/clamp (:hunks file) [s e])]
          (if hs [hs hs] [s s]))))))

(defn place
  "Where one note goes: `{:inline {...github comment...}}` or `{:body reason}`."
  [report note]
  (let [file (some #(when (spells? (some-> (:file note) str/trim) (:path %)) %) (:clj report))
        loc (if file (locate file note) {:error (str "no changed Clojure file " (pr-str (:file note)))})]
    (if (map? loc)
      {:note note :body (:error loc)}
      (if-let [[s e] (hunks/clamp (:hunks file) loc)]
        {:note note
         :inline (cond-> {:path (:path file) :line e :side "RIGHT" :body (:body note)}
                   (< s e) (assoc :start_line s :start_side "RIGHT"))}
        {:note note :body (str "lines " (first loc) "–" (second loc) " are outside the PR's diff")}))))

(defn- body-line [{:keys [note body]}]
  (str "- `" (:file note) "`"
       (when (:form note) (str " — `" (:form note) (when (:at note) (str " › " (:at note))) "`"))
       ": " (:body note) "\n  _(" body ")_"))

(defn draft
  "The GitHub review payload for `notes` on a PR report, plus where each note went.
  `verdict` is approve, request-changes or comment."
  [report verdict summary notes]
  (let [event (or (events (name verdict)) (throw (ex-info (str "verdict must be one of " (str/join ", " (keys events))) {:verdict verdict})))
        placed (binding [*names* (:names report)] (mapv #(place report %) notes))
        inline (keep :inline placed)
        outside (filter :body placed)
        body (str/trim (str summary
                            (when (seq outside)
                              (str "\n\n**Notes outside the diff**\n\n" (str/join "\n" (map body-line outside))))))]
    (when (and (not= event "APPROVE") (str/blank? body) (empty? inline))
      (throw (ex-info "a comment or request-changes review needs a summary or at least one note" {})))
    {:payload (cond-> {:commit_id (:head report) :event event :comments (vec inline)}
                (not (str/blank? body)) (assoc :body body))
     :placed placed}))
