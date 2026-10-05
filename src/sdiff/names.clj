(ns sdiff.names
  "Short names for one pull request. A path or namespace is shown by the
  shortest suffix no other path or namespace in the PR ends with; a namespace
  the PR's own files alias is shown by that alias. Two-segment namespaces, which
  is where registry dev-ids live, keep their full name. The table is
  deterministic, so a short name means the same thing everywhere on the page,
  and a legend maps each one back."
  (:require [clojure.string :as str]
            [sdiff.kondo :as kondo]))

(defn- unique-suffixes
  "{full shortest-suffix} over `names` split by `sep-re` and joined by `sep`."
  [names sep-re sep]
  (let [parts (into {} (for [n names] [n (str/split n sep-re)]))
        suffix (fn [n k] (str/join sep (take-last k (parts n))))]
    (into {}
          (for [n names]
            [n (or (some (fn [k] (let [s (suffix n k)]
                                   (when (not-any? #(and (not= % n) (str/ends-with? (str sep %) (str sep s))) names) s)))
                         (range 1 (count (parts n))))
                   n)]))))

(def ^:private ns-form-re #"\(ns\s+(?:\^\S+\s+)*([\w.\-]+)")
(def ^:private alias-re #"\[([\w.\-]+)\s+(?:[^\[\]]*?\s)?:as\s+([\w.\-]+)")
(def ^:private qualified-re #"(?<![\w.\-:/])::?([a-zA-Z][\w\-]*(?:\.[\w\-]+){2,})/")


(defn- segments [ns] (count (str/split ns #"\.")))

(defn- choose
  "Short names: the alias the PR's source files give a namespace most often,
  else its shortest unique suffix. Test files' aliases (`SUT`, `sut`) are not
  names."
  [nss alias-of]
  (let [suffix (unique-suffixes (vec nss) #"\." ".")
        taken (atom #{})]
    (into {} (for [n (sort nss)
                   :let [a (alias-of n)
                         s (if (and a (not (@taken a)) (not (nss a))) a (suffix n))]]
               (do (swap! taken conj s) [n s])))))

(defn- majority-alias [usages]
  (into {} (for [[lib us] (group-by :to usages)]
             [(str lib) (str (key (first (sort-by (fn [[a n]] [(- n) (- (count (str a))) (str a)]) (frequencies (map :alias us))))))])))

(defn- from-analysis [a clj]
  (let [test? (fn [path] (re-find #"(^|/)test/|_test\.clj" path))
        usages (filter :alias (:namespace-usages a))
        alias-of (majority-alias (remove #(test? (:filename %)) usages))
        nss (set (filter #(>= (segments %) 3)
                         (map str (concat (map :name (:namespace-definitions a))
                                          (map :to (:namespace-usages a))
                                          (keep :ns (:keywords a))))))]
    (choose nss alias-of)))

(defn table
  "`{:paths {full short} :nss {full short}}` for a report. Namespaces and their
  aliases come from clj-kondo's analysis of the PR's sources when it is
  available, else from reading the `ns` forms."
  [{:keys [clj other] :as report}]
  (let [analysis (kondo/analyse (into {} (for [f clj s [:new :old] :when (seq (s f))]
                                            [(str (name s) "/" (:path f)) (s f)])))
        test? (fn [f] (re-find #"(^|/)test/|_test\.clj" (:path f)))
        srcs-of (fn [fs] (for [f fs s [(:old f) (:new f)] :when (seq s)] s))
        srcs (srcs-of clj)
        own (set (keep #(second (re-find ns-form-re %)) srcs))
        alias-counts (fn [ss] (frequencies (for [s ss [_ lib a] (re-seq alias-re s)] [lib a])))
        best (fn [counts] (into {} (for [[lib pairs] (group-by ffirst counts)]
                                     [lib (ffirst (sort-by (fn [[a n]] [(- n) (- (count a)) a]) (map (fn [[[_ a] n]] [a n]) pairs)))])))
        alias-of (best (alias-counts (srcs-of (remove test? clj))))
        mentioned (set (for [s srcs [_ ns] (re-seq qualified-re s)] ns))
        nss (set (filter #(>= (segments %) 3) (concat own mentioned (keys alias-of))))
        ns-short (if analysis (from-analysis analysis clj) (choose nss alias-of))
        paths (vec (distinct (concat (map :path clj) (map :path other))))]
    {:paths (into {} (remove (fn [[k v]] (= k v)) (unique-suffixes paths #"/" "/")))
     :nss (into {} (remove (fn [[k v]] (= k v)) ns-short))}))

(defn- replacer [{:keys [paths nss]}]
  (let [ps (sort-by (comp - count key) paths)
        ns (sort-by (comp - count key) nss)
        p-re (when (seq ps) (re-pattern (str "(?<![\\w/.\\-])(" (str/join "|" (map (comp java.util.regex.Pattern/quote key) ps)) ")(?![\\w/\\-])")))
        n-re (when (seq ns) (re-pattern (str "(?<![\\w.\\-])(" (str/join "|" (map (comp java.util.regex.Pattern/quote key) ns)) ")(?=/|[\\s)\\]}\"',]|$)")))]
    (fn [s]
      (cond-> s
        p-re (str/replace p-re #(get paths (second %) (first %)))
        n-re (str/replace n-re #(get nss (second %) (first %)))))))

(defn shorten-text [t s] (if (and t (string? s)) ((replacer t) s) s))

(defn shorten-hiccup
  "Rewrites strings in a hiccup tree, leaving attribute maps and source blocks
  (`:pre`) alone, so data attributes and code stay exact."
  [t h]
  (if-not t
    h
    (let [r (replacer t)]
      (letfn [(walk* [x]
                (cond (string? x) (r x)
                      (map? x) x
                      (and (vector? x) (keyword? (first x)) (str/starts-with? (name (first x)) "pre")) x
                      (vector? x) (mapv walk* x)
                      (seq? x) (map walk* x)
                      :else x))]
        (walk* h)))))

(defn used
  "The part of the table whose full names occur in `text`."
  [t text]
  (when t
    (let [in? (fn [[full _]] (str/includes? text full))]
      {:paths (into {} (filter in? (:paths t))) :nss (into {} (filter in? (:nss t)))})))

(defn hiccup-strings [h]
  (cond (string? h) [h]
        (map? h) []
        (and (vector? h) (keyword? (first h)) (str/starts-with? (name (first h)) "pre")) []
        (sequential? h) (mapcat hiccup-strings h)
        :else []))

(defn legend
  "Hiccup for the legend, or nil when nothing was shortened."
  [{:keys [paths nss]}]
  (let [rows (concat (sort-by val nss) (sort-by val paths))]
    (when (seq rows)
      [:details.legend
       [:summary (str "Names on this page are shortened: " (count rows) " short names")]
       [:table.legend-table [:tbody (for [[full short] rows] [:tr [:td [:code short]] [:td [:code.mute full]]])]]])))

(defn legend-text [{:keys [paths nss]}]
  (let [rows (concat (sort-by val nss) (sort-by val paths))]
    (when (seq rows)
      (str "names: " (str/join ", " (for [[full short] rows] (str short " = " full))) "\n"))))
