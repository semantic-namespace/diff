(ns sdiff.core
  "Structural diff of Clojure source: two versions of a file become a list of
  changes named by their place in the code (the binding, clause or step they
  live in) rather than by line.

  The unit is the top-level form. Forms are paired by identity (`top-id`), their
  children by role (`children-ids`), and whatever cannot be paired by role is
  aligned by longest common subsequence of subtree hashes. A change that keeps
  some sub-expressions verbatim is reported as `:reshaped` with the kept parts,
  an expression that moves between slots as a move, and a new function whose body
  came out of an existing expression as an extraction, with the drift between
  the two.

  Whitespace and comments never produce a semantic change; `#_` forms do, since
  unevaluating code changes behaviour."
  (:refer-clojure :exclude [short])
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn skip?
  "Whitespace and comments are skipped; `#_` uneval is kept because it is semantic."
  [node]
  (or (n/whitespace? node) (n/comment? node)))

(defn kids [node] (if (n/inner? node) (vec (remove skip? (n/children node))) []))

(defn head
  "The operator symbol of a list form, as a string, or nil."
  [node]
  (when (and (= :list (n/tag node)) (seq (kids node)))
    (let [h (first (kids node))] (when (= :token (n/tag h)) (n/string h)))))

(def h
  "Structural hash of a node: tag plus children, or tag plus text for a leaf."
  (memoize (fn [node] (hash [(n/tag node) (if (seq (kids node)) (mapv h (kids node)) (n/string node))]))))

(defn subtrees
  "Hashes of every inner node with at least two children."
  [node]
  (if (>= (count (kids node)) 2) (into #{(h node)} (mapcat subtrees (kids node))) #{}))

(defn own-comments [node] (if (n/inner? node) (mapv n/string (filter n/comment? (n/children node))) []))

(defn short [s] (let [s (str/replace s #"\s+" " ")] (if (> (count s) 60) (str (subs s 0 57) "…") s)))

(defn unwrap
  "A form wrapped in a reader conditional is represented by its first list branch."
  [node]
  (if (= :reader-macro (n/tag node))
    (let [inner (kids (second (kids node)))] (or (some #(when (= :list (n/tag %)) %) inner) node))
    node))

(def binding-forms #{"let" "loop" "binding" "when-let" "if-let" "when-some" "if-some" "with-open" "doseq" "for" "with-redefs"})
(def defn-forms #{"defn" "defn-" "fn" "defmacro" "defmethod"})
(def thread-forms #{"->" "->>" "some->" "some->>" "cond->" "cond->>" "as->"})
(def special-heads
  "Operators whose replacement changes control flow or binding, so a swap of one
  for another is a restructure rather than a rename of the operator."
  (into #{"if" "if-not" "when" "when-not" "cond" "case" "do" "try" "catch" "finally" "throw"
          "and" "or" "not" "recur" "quote" "var" "def" "ns" "while" "when-first" "letfn" "delay" "future"}
        (concat binding-forms defn-forms thread-forms)))

(defn top-id
  "Identity of a top-level form: what pairs it with its counterpart on the other side."
  [node]
  (let [[hd a b] (kids (unwrap node))]
    (case (some-> hd n/string)
      "ns"        ["ns" (n/string a)]
      "defmethod" ["defmethod" (n/string a) (n/string b)]
      nil         [(name (n/tag node)) (short (n/string node))]
      [(n/string hd) (when a (n/string a))])))

(defn label [k] (or (head k) (name (n/tag k))))

(defn children-ids
  "The children of a form as `[[id child] ...]`, where the id is the role the diff
  pairs children by and prints as a path step. Returns `::lcs` for vectors and
  sets (pair by content) and `::seq` for generic forms (pair by LCS then order)."
  [node]
  (let [ks (kids node) hd (head node)]
    (cond
      (and (binding-forms hd) (= :vector (n/tag (second ks))))
      (let [[_ bv & body] ks]
        (concat (map (fn [[sym expr]] [["binding" (short (n/string sym))] expr]) (partition 2 (kids bv)))
                (map-indexed (fn [i b] [[(if (= 1 (count body)) "body" (str "body " (inc i)))] b]) body)))

      (#{"when" "when-not" "if" "if-not" "while" "when-first"} hd)
      (map vector
           (map vector (cons "test" (if (#{"if" "if-not"} hd) ["then" "else"] (map #(str "body " (inc %)) (range)))))
           (rest ks))

      (= hd "cond")
      (map-indexed (fn [i [t e]] [[(str "clause " (inc i))] (n/list-node [t e])])
                   (partition 2 (rest ks)))

      (= hd "case")
      (concat [[["expr"] (second ks)]]
              (map (fn [[c e]] [["case" (short (n/string c))] e]) (partition 2 (nnext ks))))

      (thread-forms hd) ::seq

      (defn-forms hd)
      (let [bodies (filter #(= :list (n/tag %)) ks)
            multi? (and (> (count bodies) 1) (every? #(= :vector (n/tag (first (kids %)))) bodies))]
        (if multi?
          (map (fn [b] [["arity" (count (kids (first (kids b))))] b]) bodies)
          (let [argv (first (filter #(= :vector (n/tag %)) ks))
                i (.indexOf ^java.util.List ks argv)
                body (when (pos? i) (subvec ks (inc i)))]
            (concat (when argv [[["args"] argv]])
                    (map-indexed (fn [j b] [[(if (= 1 (count body)) "body" (str "body " (inc j)))] b]) body)))))

      (= :map (n/tag node))
      (map (fn [[k v]] [["key" (n/string k)] v]) (partition 2 ks))

      (#{:vector :set} (n/tag node))
      ::lcs

      :else ::seq)))

(declare diff kept-subtrees inner-nodes maximal)

(defn link-moves
  "Within one alignment, an old expression that lost its slot and a new one that
  appeared may share subtrees: that is a move, recorded on both ends."
  [path res]
  (let [here (fn [c] (and (= (count (:path c)) (inc (count path))) (not (:moved-to c)) (not (:moved-from c))))
        outs (filter #(and (here %) (#{:replaced :removed :reshaped} (:op %)) (:old %)) res)
        ins  (filter #(and (here %) (#{:added :replaced :reshaped} (:op %)) (:new %)) res)
        links (for [o outs i ins :when (not= (:path o) (:path i))
                    :let [k (kept-subtrees (:old o) (:new i))] :when (seq k)] [o i k])]
    (reduce (fn [res [o i k]]
              (mapv (fn [c] (cond (= c o) (assoc c :moved-to (:path i) :kept (into (vec (:kept c)) k))
                                  (= c i) (assoc c :moved-from (:path o) :kept (into (vec (:kept c)) k))
                                  :else c)) res))
            (vec res) links)))

(defn align [path ida idb]
  (let [ma (into {} ida) mb (into {} idb)]
    (link-moves path
                (mapcat (fn [id]
                          (let [a (ma id) b (mb id)
                                leaf (or a b)
                                step (if (and (str/starts-with? (str (first id)) "arg ") (empty? (kids leaf))) [(short (n/string leaf))] id)]
                            (cond (and a b) (diff (conj path step) a b)
                                  a         [{:op :removed :path (conj path step) :old a}]
                                  :else     [{:op :added   :path (conj path step) :new b}])))
                        (distinct (concat (map first ida) (map first idb)))))))

(defn pattern-syms
  "Symbols bound by a destructuring pattern."
  [pat]
  (cond (= :token (n/tag pat)) (let [x (n/string pat)] (when (and (re-matches #"[A-Za-z_*+!?<>=.$-][^\s()\[\]{}\"]*" x) (not (#{"&" "_"} x))) [x]))
        (n/inner? pat) (mapcat pattern-syms (kids pat))
        :else []))

(defn bound-syms
  "Every symbol bound somewhere inside this form by let, fn, defn or catch-style
  binders. Only these are eligible for alpha-renaming; vars stay literal."
  [node]
  (let [hd (head node) ks (kids node)
        here (cond (and (binding-forms hd) (= :vector (n/tag (second ks))))
                   (mapcat (fn [[p _]] (pattern-syms p)) (partition 2 (kids (second ks))))
                   (defn-forms hd)
                   (let [bodies (filter #(= :list (n/tag %)) ks)]
                     (concat (mapcat pattern-syms (filter #(= :vector (n/tag %)) ks))
                             (mapcat #(when (= :vector (n/tag (first (kids %)))) (pattern-syms (first (kids %)))) bodies)))
                   (= hd "catch") (pattern-syms (nth ks 2 (n/token-node 'x)))
                   :else [])]
    (into (set here) (mapcat bound-syms (kids node)))))

(defn canon
  "Canonical structure of a node with `locals` replaced by their first-appearance
  index. Returns `[structure sym-order]`; two nodes match modulo renaming when
  their structures are equal, and zipping the orders gives the rename mapping."
  [node locals]
  (let [order (atom [])
        walk (fn walk [x]
               (cond (seq (kids x)) (into [(n/tag x)] (map walk (kids x)))
                     (and (= :token (n/tag x)) (locals (n/string x)))
                     (let [s (n/string x)
                           i (let [k (.indexOf ^java.util.List @order s)] (if (neg? k) (do (swap! order conj s) (dec (count @order))) k))]
                       [:local i])
                     :else (n/string x)))]
    [(walk node) @order]))

(defn alpha-kept
  "Like `kept-subtrees` but modulo renaming of locals. Each entry is
  `[old new {:renamed {old-sym new-sym}}]`."
  [a b locals-a locals-b]
  (let [cb (into {} (for [x (inner-nodes b) :let [[st ord] (canon x locals-b)]] [st [x ord]]))
        matches (for [x (inner-nodes a) :let [[st ord] (canon x locals-a)] :when (cb st)
                      :let [[y ordb] (cb st) ren (into {} (remove (fn [[o nw]] (= o nw)) (map vector ord ordb)))]]
                  [x y {:renamed ren}])
        matches (vals (into {} (map (fn [[x :as m]] [(h x) m]) matches)))
        maxi (set (map h (maximal (map first matches))))]
    (vec (sort-by #(- (count (n/string (first %)))) (filter #(maxi (h (first %))) matches)))))

(defn inner-nodes [node] (if (>= (count (kids node)) 2) (cons node (mapcat inner-nodes (kids node))) []))

(defn maximal
  "Drop nodes contained in another node of the set."
  [nodes]
  (remove (fn [x] (some #(and (not= % x) (contains? (set (map h (inner-nodes %))) (h x)) (not= (h %) (h x))) nodes)) nodes))

(defn kept-subtrees
  "Sub-expressions of `a` that survive verbatim in `b`, as `[[old-node new-node] ...]`,
  largest first, non-nested."
  [a b]
  (let [hb (into {} (map (juxt h identity) (inner-nodes b)))
        shared (filter #(hb (h %)) (inner-nodes a))
        shared (maximal (vals (into {} (map (juxt h identity) shared))))]
    (vec (for [x (sort-by #(- (count (n/string %))) shared)] [x (hb (h x))]))))

(defn lcs-pairs
  "Indices `[i j]` of a longest common subsequence of xs and ys, by hash."
  [xs ys]
  (let [n (count xs) m (count ys)
        t (make-array Long/TYPE (inc n) (inc m))]
    (doseq [i (range (dec n) -1 -1) j (range (dec m) -1 -1)]
      (aset t i j (long (if (= (h (xs i)) (h (ys j))) (inc (aget t (inc i) (inc j)))
                            (max (aget t (inc i) j) (aget t i (inc j)))))))
    (loop [i 0 j 0 acc []]
      (cond (or (= i n) (= j m)) acc
            (= (h (xs i)) (h (ys j))) (recur (inc i) (inc j) (conj acc [i j]))
            (>= (aget t (inc i) j) (aget t i (inc j))) (recur (inc i) j acc)
            :else (recur i (inc j) acc)))))

(defn lcs-align [path a b]
  (let [xs (kids a) ys (kids b) pairs (set (lcs-pairs xs ys))
        ri (set (map first pairs)) rj (set (map second pairs))]
    (concat (for [i (range (count xs)) :when (not (ri i))] {:op :removed :path (conj path [(str "#" (inc i) " " (short (n/string (xs i))))]) :old (xs i)})
            (for [j (range (count ys)) :when (not (rj j))] {:op :added :path (conj path [(str "#" (inc j) " " (short (n/string (ys j))))]) :new (ys j)}))))

(defn seq-align
  "Children of a generic form: exact matches by LCS first, then leftovers paired
  in order, then the remainder as additions and removals."
  [path a b]
  (let [hd (head a)
        xs (if hd (vec (rest (kids a))) (kids a)) ys (if hd (vec (rest (kids b))) (kids b))
        pairs (lcs-pairs xs ys) ri (set (map first pairs)) rj (set (map second pairs))
        lo (remove ri (range (count xs))) ln (remove rj (range (count ys)))
        zipped (map vector lo ln)
        extra-old (drop (count zipped) lo) extra-new (drop (count zipped) ln)
        step (fn [k i] [(cond (head k) (str (head k) (when (> (count xs) 1) (str " " (inc i))))
                              (seq (kids k)) (str (name (n/tag k)) " " (inc i))
                              :else (short (n/string k)))])]
    (concat (mapcat (fn [[i j]] (diff (conj path (step (xs i) i)) (xs i) (ys j))) zipped)
            (for [i extra-old] {:op :removed :path (conj path (step (xs i) i)) :old (xs i)})
            (for [j extra-new] {:op :added :path (conj path (step (ys j) j)) :new (ys j)}))))

(defn diff
  "Changes between nodes `a` and `b` under `path`. Argument vectors pair by
  position so a renamed parameter is one change, not a removal plus an addition;
  a defn docstring change is reported as `:comments`; two calls that differ only
  in their operator are one change at `head`, so a function or keyword renamed
  at many call sites rolls up into a single rename."
  [path a b]
  (cond
    (= (h a) (h b))
    (if (= (own-comments a) (own-comments b)) [] [{:op :comments :path path}])

    (and (seq (kids a)) (seq (kids b)) (= (n/tag a) (n/tag b)) (= (head a) (head b)))
    (if (#{::lcs ::seq} (children-ids a))
      (cond
        (= ::seq (children-ids a)) (vec (link-moves path (seq-align path a b)))
        (= ["args"] (last path))
        (vec (align path (map-indexed (fn [i k] [[(str "#" (inc i))] k]) (kids a))
                    (map-indexed (fn [i k] [[(str "#" (inc i))] k]) (kids b))))
        :else (vec (lcs-align path a b)))
      (let [ida (children-ids a) idb (children-ids b)
            dstr (fn [x] (let [[_ _ s] (kids x)] (when (and s (or (= :multi-line (n/tag s)) (str/starts-with? (n/string s) "\""))) s)))
            res (align path ida idb)
            res (if (and (defn-forms (head a)) (not= (some-> (dstr a) n/string) (some-> (dstr b) n/string)))
                  (cons {:op :comments :path (conj path ["docstring"])} res) res)]
        (cond-> (vec res)
          (not= (own-comments a) (own-comments b)) (conj {:op :comments :path path}))))

    (and (head a) (head b) (not (special-heads (head a))) (not (special-heads (head b)))
         (= (map h (rest (kids a))) (map h (rest (kids b)))))
    [{:op :replaced :path (conj path ["head"]) :old (first (kids a)) :new (first (kids b))}]

    :else
    (let [kept (kept-subtrees a b)]
      (if (seq kept)
        [{:op :reshaped :path path :old a :new b :kept kept}]
        [{:op :replaced :path path :old a :new b}]))))

(defn forms [src] (remove skip? (n/children (p/parse-string-all src))))
(defn index [src] (into {} (map (juxt top-id identity) (forms src))))

(defn arity-body [defn-node nargs]
  (let [bodies (filter #(= :list (n/tag %)) (kids defn-node))
        pick (some #(when (= nargs (count (kids (first (kids %))))) %) bodies)]
    (if pick (last (kids pick)) (last (kids defn-node)))))

(defn- tokens [x] (if (seq (kids x)) (mapcat tokens (kids x)) [(n/string x)]))

(defn file-report
  "Structural report for one file given its old and new source. Returns
  `{:path :old :new :verdict :forms}` where verdict is `:semantic`,
  `:comments-only` or `:whitespace-only` (`:rename-only` is assigned later by
  `rollup-renames`, which needs the whole change set).

  An extraction is detected when a replaced expression calls, or passes as a
  value, a function new in this file whose body shares a subtree with the old
  expression, verbatim or modulo renamed locals. A form whose only changes are
  in its argument vector is tagged signature-only."
  [path old new]
  (let [ia (index old) ib (index new)
        ids (distinct (concat (keys ia) (keys ib)))
        added (into {} (for [id ids :when (and (ib id) (not (ia id)))] [(second id) (ib id)]))
        base (for [id ids :let [a (ia id) b (ib id)]]
               (cond (and a b) {:id id :changes (cond-> (vec (diff [] (unwrap a) (unwrap b)))
                                                  (not= (n/tag a) (n/tag b))
                                                  (conj {:op :wrapper :path [] :old a :new b}))}
                     a {:id id :changes [{:op :removed-form :old a}]}
                     :else {:id id :changes [{:op :added-form :new b}]}))
        extractions (for [{:keys [id changes]} base
                          {:keys [op old new path]} changes
                          :let [fname (when (and (= op :replaced) new) (first (filter added (tokens new))))
                                f (when fname (added fname))]
                          :when f
                          :let [body (arity-body f (if (= (head new) fname) (dec (count (kids new))) -1))
                                verbatim (seq (set/intersection (subtrees old) (subtrees body)))
                                alpha (when-not verbatim (alpha-kept old body (bound-syms (ia id)) (bound-syms f)))
                                ren (apply merge (map #(:renamed (nth % 2)) alpha))]
                          :when (or verbatim (seq alpha))
                          :let [[x y] (if verbatim [old body] (first alpha))
                                drift (diff [] x y)
                                drift (if (seq ren)
                                        (remove (fn [{:keys [op old new]}] (and (= op :replaced) old new (= :token (n/tag old)) (= (ren (n/string old)) (n/string new)))) drift)
                                        drift)
                                drift (cond-> (vec drift) (not= (h x) (h old)) (conj {:op :dropped :path [] :old old :kept [[x y]]}))]]
                      {:fn fname :from id :from-path path :drift drift :renamed ren :kept [[x y]]})
        extractions (vals (into {} (map (juxt :fn identity) extractions)))
        ext-by-fn (into {} (map (juxt :fn identity) extractions))
        base (map (fn [{:keys [changes] :as r}]
                    (assoc r :changes (mapv (fn [c] (if-let [e (and (= :replaced (:op c)) (:new c)
                                                                    (some ext-by-fn (tokens (:new c))))]
                                                      (assoc c :extracted (:fn e) :kept (into (vec (:kept c)) (:kept e))) c)) changes)))
                  base)
        base (map (fn [{:keys [id] :as r}]
                    (if-let [e (and (= :added-form (:op (first (:changes r)))) (ext-by-fn (second id)))]
                      (assoc r :extraction e) r)) base)
        res (remove #(empty? (:changes %)) base)
        res (map (fn [{:keys [changes] :as r}]
                   (cond-> r (and (seq changes) (every? #(= ["args"] (first (:path %))) changes))
                     (assoc :note "signature only: parameters changed, body did not")))
                 res)
        sem? (fn [r] (some #(not= :comments (:op %)) (:changes r)))]
    {:path path
     :old old :new new
     :verdict (cond (empty? res) :whitespace-only
                    (not-any? sem? res) :comments-only
                    :else :semantic)
     :forms (vec res)}))

(defn tok
  "The `[old new]` token pair of a token-to-token replacement, or nil. A `_`
  parameter coming into use is not a rename."
  [{:keys [op old new]}]
  (when (and (= op :replaced) old new (= :token (n/tag old)) (= :token (n/tag new))
             (not (str/starts-with? (n/string old) "_")))
    [(n/string old) (n/string new)]))

(defn rollup-renames
  "Identical token-to-token replacements seen at two or more sites across the
  change set become one rename; a semantic file whose changes are all part of
  renames becomes `:rename-only`. Returns `{:files :renames}`."
  [files]
  (let [counts (frequencies (for [f files r (:forms f) c (:changes r) :let [k (tok c)] :when k] k))
        ren (into {} (filter (fn [[_ c]] (>= c 2)) counts))
        mark (fn [c] (if-let [k (and (ren (tok c)) (tok c))] (assoc c :rename k) c))
        files (mapv (fn [f] (update f :forms (fn [fs] (mapv #(update % :changes (fn [cs] (mapv mark cs))) fs)))) files)
        sem? (fn [r] (some #(and (not= :comments (:op %)) (not (:rename %))) (:changes r)))
        files (mapv (fn [f] (cond-> f (and (= :semantic (:verdict f)) (not-any? sem? (:forms f))) (assoc :verdict :rename-only))) files)]
    {:files files
     :renames (mapv (fn [[[o nw] c]] {:from o :to nw :count c}) (sort-by (comp - val) ren))}))

(defn fmt-path [p] (str/join " › " (map (fn [x] (if (vector? x) (str/join " " (map str x)) (str x))) p)))
