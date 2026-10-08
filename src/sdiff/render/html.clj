(ns sdiff.render.html
  "A self-contained HTML page for one or more pull requests. Files are sorted by
  verdict and only behaviour-changing files are expanded; each form is shown
  whole, with the changed and kept parts marked in the source."
  (:refer-clojure :exclude [short])
  (:require [rewrite-clj.node :as n]
            [clojure.string :as str]
            [hiccup2.core :as hc]
            [sdiff.core :refer [fmt-path head short index]]
            [sdiff.decorate :as decorate]
            [sdiff.names :as names]))

(defn- offsets [src]
  (let [starts (reductions + 0 (map #(inc (count %)) (str/split src #"\n" -1)))]
    (fn [row col] (+ (nth starts (dec row)) (dec col)))))

(defn- range-of [off node]
  (let [{:keys [row col end-row end-col]} (meta node)]
    (when row [(off row col) (off end-row end-col)])))

(defn- token? [cls] (str/starts-with? cls "t-"))

(defn- change-mark? [cls] (not (or (= cls "kept") (token? cls))))

(defn- render-ranges
  "Wrap `[s e cls]` ranges (sorted, nesting allowed, no partial overlap) over
  src[from,to): change marks as `mark`, syntax tokens (`t-*`) as `span`."
  [src from to rs]
  (loop [pos from rs rs out []]
    (if-let [[s e cls] (first rs)]
      (let [inner (take-while (fn [[s2 e2]] (and (>= s2 s) (<= e2 e))) (rest rs))
            after (drop (count inner) (rest rs))]
        (recur e after (conj out (subs src pos s) (into [(if (token? cls) :span :mark) {:class cls}] (render-ranges src s e inner)))))
      (conj out (subs src pos to)))))

(defn- range-order [[s e cls]] [s (- e) (if (token? cls) 1 0)])

(def ^:private special-forms
  #{"def" "defn" "defn-" "defmacro" "defmethod" "defmulti" "defprotocol" "defrecord" "deftype" "defonce" "deftest"
    "ns" "let" "letfn" "fn" "fn*" "if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-some" "when-first"
    "cond" "condp" "case" "do" "loop" "recur" "try" "catch" "finally" "throw" "for" "doseq" "dotimes" "while"
    "->" "->>" "some->" "some->>" "cond->" "cond->>" "as->" "and" "or" "binding" "with-open" "with-redefs"
    "reify" "extend-protocol" "extend-type" "proxy" "comment" "quote" "var" "declare" "require" "import"
    "refer-clojure" "delay" "future" "lazy-seq" "testing" "is"})

(defn- token-class [text]
  (cond (str/starts-with? text ":") "t-kw"
        (str/starts-with? text "\"") "t-str"
        (str/starts-with? text "\\") "t-str"
        (re-find #"^[+-]?\d" text) "t-num"
        (#{"nil" "true" "false"} text) "t-lit"
        (special-forms text) "t-special"))

(defn- token-marks
  "Syntax classes for the leaves of `form`: keywords, strings, numbers,
  comments, discarded forms, special forms and the head of each list."
  [form]
  (let [out (volatile! [])
        skip #{:whitespace :newline :comma :comment}]
    (letfn [(walk [node head?]
              (case (n/tag node)
                (:comment :uneval) (vswap! out conj [node "t-cmt"])
                :regex (vswap! out conj [node "t-str"])
                :token (when-let [cls (or (token-class (n/string node)) (when head? "t-head"))]
                         (vswap! out conj [node cls]))
                (when (n/inner? node)
                  (let [kids (n/children node)
                        head (first (remove (comp skip n/tag) kids))]
                    (doseq [k kids] (walk k (and (= :list (n/tag node)) (identical? k head))))))))]
      (walk form false))
    @out))

(defn- highlighted
  "Source of `form` with `marks` (`[node class]`) wrapped in spans; marks may nest.
  Synthetic nodes without position metadata are skipped."
  [src form marks]
  (let [off (offsets src)
        [fs fe] (range-of off form)
        rs (sort-by range-order
                    (for [[node cls] (concat marks (token-marks form)) :let [r (range-of off node)] :when r
                          :let [[s e] r] :when (and (>= s fs) (<= e fe))] [s e cls]))]
    (render-ranges src fs fe rs)))

(def ^:private change-classes #{"add" "del" "ext" "ren"})

(defn- split-lines
  "Highlighted hiccup cut into source lines. Each line keeps the elements that
  span it, reopened on every line, and lists its text with the change mark it
  sits in, so a line can be told changed, whole-line added or removed."
  [nodes]
  (letfn [(go [nodes mark]
            (reduce (fn [lines node]
                      (let [parts (cond
                                    (string? node) (mapv (fn [t] {:h (if (seq t) [t] []) :segs [[t mark]]}) (str/split node #"\n" -1))
                                    (and (vector? node) (keyword? (first node)))
                                    (let [[tag attrs & kids] node
                                          cls (:class attrs)
                                          m (if (and (= :mark tag) (or (change-classes cls) (= "kept" cls))) cls mark)]
                                      (mapv (fn [l] (assoc l :h (if (seq (:h l)) [(into [tag attrs] (:h l))] [])))
                                            (go kids m)))
                                    (sequential? node) (go node mark)
                                    :else [{:h [] :segs []}])
                            l (peek lines)]
                        (into (assoc lines (dec (count lines)) {:h (into (:h l) (:h (first parts))) :segs (into (:segs l) (:segs (first parts)))})
                              (rest parts))))
                    [{:h [] :segs []}] nodes))]
    (go nodes nil)))

(defn- form-lines
  "The lines of `form` in `src`, from the start of its first line, each with its
  number, hiccup, plain text, whether it carries a change and its whole-line tint."
  [src form marks]
  (let [off (offsets src)
        {:keys [row]} (meta form)
        [fs fe] (range-of off form)
        start (off row 1)
        rs (sort-by range-order
                    (for [[node cls] (concat marks (token-marks form)) :let [r (range-of off node)] :when r
                          :let [[s e] r] :when (and (>= s fs) (<= e fe))] [s e cls]))]
    (vec (map-indexed
          (fn [i {:keys [h segs]}]
            (let [text (apply str (map first segs))
                  inked (remove #(str/blank? (first %)) segs)]
              {:n (+ row i) :h h :text text
               :changed? (boolean (some #(change-classes (second %)) inked))
               :tint (when (seq inked)
                       (cond (every? #(#{"add" "ext" "ren"} (second %)) inked) "ln-add"
                             (every? #(= "del" (second %)) inked) "ln-del"))}))
          (split-lines (render-ranges src start fe rs))))))

(defn- align
  "Pairs `[i j]` of base and head line indexes, matching lines whose text is the
  same and leaving `nil` where one side has a line the other has not."
  [a b]
  (let [na (count a) nb (count b) ka (mapv #(str/trim (:text %)) a) kb (mapv #(str/trim (:text %)) b)]
    (if (> (* na nb) 2000000)
      (vec (for [i (range (max na nb))] [(when (< i na) i) (when (< i nb) i)]))
      (let [t (make-array Long/TYPE (inc na) (inc nb))]
        (doseq [i (range (dec na) -1 -1) j (range (dec nb) -1 -1)]
          (aset t i j (if (= (ka i) (kb j))
                        (inc (aget t (inc i) (inc j)))
                        (max (aget t (inc i) j) (aget t i (inc j))))))
        (->> (loop [i 0 j 0 out []]
               (cond (and (< i na) (< j nb) (= (ka i) (kb j))) (recur (inc i) (inc j) (conj out [i j]))
                     (and (< i na) (or (>= j nb) (>= (aget t (inc i) j) (aget t i (inc j))))) (recur (inc i) j (conj out [i nil]))
                     (< j nb) (recur i (inc j) (conj out [nil j]))
                     :else out))
             (partition-by (fn [[i j]] (boolean (and i j))))
             (mapcat (fn [run]
                       (if (every? (fn [[i j]] (and i j)) run)
                         run
                         (let [is (keep first run) js (keep second run) n (max (count is) (count js))]
                           (for [k (range n)] [(nth is k nil) (nth js k nil)])))))
             vec)))))

(defn- elide
  "Rows to show: every row when the form is short, else the changed rows with
  `context` rows around them; the rest become `[:run n rows]` that open on click."
  [rows changed? context]
  (if (<= (count rows) 30)
    rows
    (let [hot (set (for [[k r] (map-indexed vector rows) :when (changed? r) d (range (- context) (inc context))] (+ k d)))
          hot (if (empty? hot) (set (range 12)) hot)]
      (->> (map-indexed vector rows)
           (partition-by (fn [[k]] (contains? hot k)))
           (mapcat (fn [part] (if (contains? hot (ffirst part)) (map second part) [[:run (count part) (map second part)]])))))))

(def ^:private run-ids (atom 0))

(defn- line-div [l]
  (if l
    [:div.ln {:class (:tint l)} [:span.gut (:n l)] [:span.cd (seq (:h l))]]
    [:div.ln.spacer [:span.gut] [:span.cd " "]]))

(defn- tinted
  "`l` tinted `tint` when it has text and no tint of its own: a line only one
  side has reads as wholly removed or added."
  [l tint]
  (cond-> l (and l (nil? (:tint l)) (not (str/blank? (:text l)))) (assoc :tint tint)))

(defn- with-meta-run [[tag attrs & kids] id]
  (into [tag (-> attrs (update :class #(str/trim (str % " hid"))) (assoc :data-in id))] kids))

(defn- pane [label sha items pick]
  [:div.pane
   [:div.pane-h [:span label] [:span.sha sha]]
   [:div.scroll
    (for [it items]
      (if (and (vector? it) (= :run (first it)))
        (let [[_ n rs id] it]
          (list [:div.ln.elided {:data-run id} [:span.gut] [:span.cd (str "⋮ " n " line" (when (not= 1 n) "s"))]]
                (for [r rs] (with-meta-run (line-div (pick r)) id))))
        (line-div (pick it))))]])

(declare marks-for)

(defn- code-panes
  "Base and head of a form side by side, rows aligned, unchanged runs elided;
  one pane for a form that is only on one side."
  [old new node-old node-new changes]
  (let [sha (fn [k] (some-> decorate/*ctx* :report k (subs 0 7)))
        tag-runs (fn [items] (map #(if (and (vector? %) (= :run (first %))) (conj % (swap! run-ids inc)) %) items))]
    (cond
      (and node-old node-new)
      (let [a (form-lines old node-old (marks-for :old changes))
            b (form-lines new node-new (marks-for :new changes))
            pairs (align a b)
            changed? (fn [[i j]] (or (nil? i) (nil? j) (:changed? (a i)) (:changed? (b j))))
            items (tag-runs (elide pairs changed? 2))]
        [:div.panes
         (pane "base" (sha :base) items (fn [[i j]] (when i (cond-> (a i) (nil? j) (tinted "ln-del")))))
         (pane "head" (sha :head) items (fn [[i j]] (when j (cond-> (b j) (nil? i) (tinted "ln-add")))))])
      (or node-old node-new)
      (let [[src node side label k] (if node-new [new node-new :new "head · new form" :head] [old node-old :old "base · removed" :base])
            ls (mapv #(tinted % (if (= side :old) "ln-del" "ln-add")) (form-lines src node (marks-for side changes)))
            items (tag-runs (elide (vec (range (count ls))) (constantly false) 0))]
        [:div.panes.single (pane label (sha k) items ls)]))))

(defn- marks-for [side changes]
  (concat
   (for [{:keys [op old new rename extracted]} changes
         :let [node (if (= side :old) old new)]
         :when (and node (#{:replaced :added :removed :reshaped} op))]
     [node (cond rename "ren" extracted "ext" (= side :old) "del" :else "add")])
   (for [{:keys [kept]} changes [o nw] kept]
     [(if (= side :old) o nw) "kept"])))

(defn- kept-codes [kept] (interpose ", " (for [[o] kept] [:code.kept (short (n/string o))])))

(defn- move-note [{:keys [moved-to moved-from kept]}]
  (cond moved-to   [:span.what "moved to " [:code (fmt-path moved-to)] ": " (kept-codes kept)]
        moved-from [:span.what "takes over from " [:code (fmt-path moved-from)] ": " (kept-codes kept)]))

(defn- change-row [{:keys [op path old new extracted rename kept] :as c}]
  (let [p (fmt-path path) mv (move-note c)]
    (case op
      :wrapped [:li.chg.chg-sem [:span.p "whole form"] [:span.what "now wrapped in " [:code (or (head new) (name (n/tag new)))] "; the old form is inside it unchanged"]]
      :visibility [:li.chg.chg-sem [:span.p "visibility"] [:span.what (str (:from c) " → " (:to c))]]
      :comments [:li.chg.chg-note [:span.p (or (not-empty p) "form")] [:span.what "comments or docstring only"]]
      :wrapper  [:li.chg.chg-sem [:span.p "wrapper"] [:span.what (str (name (n/tag old)) " removed → " (name (n/tag new)))]]
      :added    [:li.chg.chg-sem [:span.p p] [:code.add (short (n/string new))] mv]
      :removed  [:li.chg.chg-sem [:span.p p] [:code.del (short (n/string old))] mv]
      :dropped  [:li.chg.chg-sem [:span.p "around the extracted part, not carried into the new function"]
                 [:code.del (short (n/string old))]]
      :reshaped [:li.chg.chg-sem [:span.p p [:em.reshape (str " restructured " (or (head old) (name (n/tag old))) " → " (or (head new) (name (n/tag new))))]
                                 (when extracted [:em.ext (str " extracted → " extracted)])]
                 [:code.del (short (n/string old))] [:code.add (short (n/string new))]
                 [:span.what "kept as-is: " (kept-codes kept)]]
      :replaced (if rename
                  [:li.chg.chg-ren [:span.p p] [:span.what "part of rename " [:code (first rename)] " → " [:code (second rename)]]]
                  [:li.chg.chg-sem [:span.p p (when extracted [:em.ext (str " extracted → " extracted)])]
                   [:code.del (short (n/string old))] [:code.add (short (n/string new))] mv])
      nil)))

(defn- with-attrs [[tag & more] attrs] (into [tag attrs] more))

(defn- brief [node] (let [t (short (n/string node))] (if (> (count t) 34) (str (subs t 0 33) "…") t)))

(defn summary
  "What a change does to a form, in a few words: the first changes by kind,
  and how many more there are."
  [{:keys [id changes]}]
  (case (:op (first changes))
    :added-form (let [h (str (first id))]
                  (str "new " (cond (re-find #"^defn-?$|^defmacro$" h) "function"
                                    (re-find #"^def(once)?$" h) "value"
                                    (re-find #"(^|/)def$" h) "spec"
                                    (re-find #"^deftest$" h) "test"
                                    (re-find #"(^|/)(register!|bind)$" h) "registration"
                                    (re-find #"^defmethod$" h) "method"
                                    (re-find #"^def(record|type)$" h) "record"
                                    (re-find #"^defprotocol$" h) "protocol"
                                    (re-find #"^ns$" h) "namespace"
                                    :else h)))
    :removed-form "removed"
    (let [step (fn [path] (let [s (last path)] (if (vector? s) (str/join " " (map str s)) (str s))))
          phrase (fn [{:keys [op path old new rename] :as c}]
                   (case op
                     :added (str "adds " (brief new))
                     :removed (str "drops " (brief old))
                     :replaced (if rename (str "renames " (first rename) " → " (second rename)) (str "changes " (if (seq path) (step path) "a value")))
                     :reshaped (str "restructures " (or (head old) (name (n/tag old))) " → " (or (head new) (name (n/tag new))))
                     :comments "comments or docstring only"
                     :visibility (str "becomes " (:to c))
                     :wrapped (str "now wrapped in " (or (head new) (name (n/tag new))))
                     :wrapper "changes its wrapper"
                     nil))
          ps (distinct (keep phrase changes))]
      (str (str/join ", " (take 2 ps)) (when (> (count ps) 2) (str ", +" (- (count ps) 2) " more"))))))

(defn form-view [{:keys [old new path]} {:keys [id was changes extraction note]}]
  (let [op0 (:op (first changes))
        full? (#{:added-form :removed-form} op0)
        node-old (when-not (= op0 :added-form) (get (index old) (or was id)))
        node-new (when-not (= op0 :removed-form) (get (index new) id))]
    [:section.form {:id (decorate/anchor {:path path} {:id id}) :data-file path :data-form (str/join " " (remove nil? (map str id)))
                    :data-status (case op0 :added-form "new" :removed-form "removed" "changed")}
     [:h3 [:code.fname {:title (str/join " " (map str id))} (str/join " " (map str id))]
      [:span.sum (summary {:id id :changes changes})]
      (case op0 :added-form [:span.tag.tag-add "new"] :removed-form [:span.tag.tag-del "removed"] nil)
      (when was [:span.tag.tag-note (str "was " (str/join " " (remove nil? (map str was))))])
      (when (some #(= :reshaped (:op %)) changes) [:span.tag.tag-note "restructured"])
      (when extraction [:span.tag.tag-ext (str "extracted from " (str/join " " (map str (:from extraction))))])
      (when note [:span.tag.tag-note note])
      [:span.fpath {:title path} path]]
     (when-not full?
       [:ul.changes (for [c changes :let [row (change-row c)] :when row]
                      (if (seq (:path c)) (with-attrs row {:data-at (fmt-path (:path c))}) row))])
     (when extraction
       [:div.drift
        (when (seq (:renamed extraction))
          [:p "Locals renamed: " (interpose ", " (for [[o nw] (:renamed extraction)] [:span [:code o] " → " [:code nw]]))])
        [:p (if (seq (:drift extraction)) "Compared with the expression it replaced, the new body differs in:" "The body is the replaced expression, unchanged.")]
        [:ul.changes (map change-row (:drift extraction))]])
     (decorate/form-decorations {:path path} {:id id})
     (decorate/form-annotations {:path path} {:id id})
     (code-panes old new node-old node-new changes)]))

(def ^:dynamic *shell* false)

(def verdict-label {:semantic "changes behaviour" :rename-only "rename only" :comments-only "comments only" :whitespace-only "formatting only"})
(def verdict-order {:semantic 0 :rename-only 1 :comments-only 2 :whitespace-only 3})

(defn file-view [gh-url pr-num {:keys [path verdict forms status moved] :as fr}]
  [:article.file {:class (name verdict) :id (str "f-" (hash path)) :data-file path}
   [:h2 [:span.verdict (verdict-label verdict)]
    [:code.path {:title path} path]
    (case status "A" [:span.tag.tag-add "new file"] "D" [:span.tag.tag-del "deleted"] nil)
    (when gh-url [:a.gh {:href (str gh-url "/pull/" pr-num "/files") :target "_blank"} "comment on GitHub"])]
   (when (= verdict :semantic)
     (let [news (count (filter #(= :added-form (:op (first (:changes %)))) forms))
           gone (count (filter #(= :removed-form (:op (first (:changes %)))) forms))]
       [:p.gsub (str (count forms) " form" (when (not= 1 (count forms)) "s") " · " (- (count forms) news gone) " changed · " news " new"
                     (when (pos? gone) (str " · " gone " removed")))]))
   (when (seq moved)
     [:ul.renames (for [id moved] [:li "moved " [:code (str/join " " (remove nil? (map str id)))] [:span.n "position among the forms changed"]])])
   (when (= verdict :semantic) (if *shell* [:div.glist (map (partial form-view fr) forms)] (map (partial form-view fr) forms)))
   (when (#{:comments-only :rename-only} verdict)
     [:details [:summary "forms touched"]
      [:ul (for [f forms] [:li [:code (str/join " " (map str (:id f)))]
                           (for [c (:changes f) :when (:rename c)] [:span.where (str " — " (fmt-path (:path c)))])])]])])

(defn- standing-word [state]
  (case state "APPROVED" "approved" "CHANGES_REQUESTED" "requested changes" "DISMISSED" "review dismissed" state))

(defn status-text
  "One line: the PR's state, then each reviewer's standing, the viewer first."
  [{:keys [state merged-at author viewer mine others]}]
  (str/join " · "
            (concat [(str state (when merged-at (str " " merged-at)))]
                    (when (= author viewer) ["your own PR"])
                    (when mine [(str "you " (standing-word (:state mine)) " " (:at mine))])
                    (for [{:keys [login state at]} others] (str login " " (standing-word state) " " at)))))

(defn- status-view [{:keys [state merged-at author viewer mine others] :as st}]
  (when st
    [:p.status
     [:span.badge {:class (str "badge-" state)} state (when merged-at (str " " merged-at))]
     (when (= author viewer) [:span.mute " · your own PR"])
     (when mine (list " · " [:a {:href (:url mine) :target "_blank"} "you " (standing-word (:state mine))] " " [:span.mute (:at mine)]))
     (for [{:keys [login state at url]} others]
       (list " · " [:a {:href url :target "_blank"} login " " (standing-word state)] " " [:span.mute at]))]))

(defn- pr-view [gh-url {:keys [num title clj other renames] :as r}]
  (let [counts (frequencies (map :verdict clj))]
    [:section.pr {:id (str "pr-" num)}
     [:header
      (when-not *shell*
        (list [:h1 (if gh-url [:a {:href (str gh-url "/pull/" num) :target "_blank"} (str "#" num)] (str "#" num)) " " title]
              (status-view (:status r))))
      [:p.sum
       (str (count clj) " Clojure file" (when (not= 1 (count clj)) "s") ": ")
       (str/join ", " (for [[k l] [[:semantic "change behaviour"] [:rename-only "rename only"] [:comments-only "comments only"] [:whitespace-only "formatting only"]] :when (counts k)] (str (counts k) " " l)))
       (when (seq other) (str "; " (count other) " other file" (when (not= 1 (count other)) "s")))]
      (when (seq renames)
        [:ul.renames (for [{:keys [from to count]} renames]
                       [:li "rename " [:code from] " → " [:code to] [:span.n (str count " sites across the PR")]])])
      (map decorate/render-annotation (decorate/annotations-on decorate/*ctx* "page"))
      (decorate/header r)]
     (map (partial file-view gh-url num) (sort-by (comp verdict-order :verdict) clj))
     (when (seq other)
       [:details.other [:summary (str (count other) " non-Clojure files" (when gh-url ", shown by GitHub"))]
        [:ul (for [{:keys [path]} other] [:li {:data-file path} (if gh-url [:a {:href (str gh-url "/pull/" num "/files") :target "_blank"} path] path)])]])]))

(def css (slurp (clojure.java.io/resource "sdiff/report.css")))

(defn page
  "Whole page for `prs`, each a report with `:num` and `:title`. `gh-url` is the
  repository URL used for links, or nil for a page without GitHub links.
  `extra-head` and `extra-body` are hiccup appended to head and body, which is
  how the local review server adds its review panel. `body` replaces the
  per-PR content, which is how a view renders over the same page."
  [gh-url repo-name prs & {:keys [extra-head extra-body body before names shell]}]
  (str "<!doctype html>"
       (hc/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1, viewport-fit=cover"}]
          [:title (str "Structural review — " repo-name)]
          [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
          [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap"}]
          [:style (hc/raw css)]
          extra-head]
         (if (and shell (= 1 (count prs)))
           (binding [*shell* true]
             (let [{:keys [num title status head author] :as r} (first prs)
                   content (or body (pr-view gh-url r))
                   used (when names (names/used names (apply str (names/hiccup-strings content))))]
               [:body.shell-page
                [:header.top
                 [:div.top-in
                  [:div.row1
                   [:h1 [:span.repo repo-name]
                    (if gh-url [:a {:href (str gh-url "/pull/" num) :target "_blank"} (str "#" num)] (str "#" num)) " " title]
                   [:div.meta
                    (when status [:span.pill {:class (str "pill-" (:state status))} (:state status) (when (:merged-at status) (str " " (:merged-at status)))])
                    [:span (str/join " · " (remove nil? [(or (:author status) author)
                                                         (when (and status (= (:author status) (:viewer status))) "your own PR")
                                                         (when head (str "head " (subs head 0 (min 7 (count head)))))]))]]
                   [:div#sd-progress.progress]]
                  [:div.row2 before]]]
                [:main.main (when used (names/legend used)) (names/shorten-hiccup used content)]
                extra-body]))
         [:body
          [:div.wrap
           [:div.intro
            [:h1 "What changed, by form"]
            [:p (str (count prs) " pull request" (when (not= 1 (count prs)) "s") " from ") [:code repo-name]
             ", read structurally. Each file is sorted into one of four verdicts; only files that change behaviour are expanded. Inside those, every change is named by its place in the code — the binding, clause or step it lives in — rather than by line."]]
           (let [content (list before
                               (or body
                                   (list [:nav.toc (for [{:keys [num title]} prs] [:a {:href (str "#pr-" num)} (str "#" num " " title)])]
                                         (map (partial pr-view gh-url) prs))))
                 used (when names (names/used names (apply str (names/hiccup-strings content))))]
             (list (when used (names/legend used))
                   (names/shorten-hiccup used content)))]
          extra-body])])))
