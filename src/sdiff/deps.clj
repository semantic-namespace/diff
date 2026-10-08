(ns sdiff.deps
  "Dependencies of a pull request's changed code, base against head, read from
  the whole repository and not only the changed files: who calls each changed
  function, what it calls, which libraries and kinds of I/O it reaches, and how
  namespaces depend on one another.

  The graph is clj-kondo's analysis over top-level forms and the protocol
  methods records and reify forms implement. A call to a protocol method with
  one implementation is a call to it; with several, a possible call. Functions
  a generator defines at load time, such as HugSQL queries, show up as
  unresolved symbols and are counted as calls to that library. Other tools add
  edges a call graph cannot see with `add-bridge!`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [sdiff.core :as core]
            [sdiff.decorate :as decorate :refer [derived]]
            [sdiff.kondo :as kondo]
            [sdiff.state :as state]))

(def defaults
  {:test-paths "(^|/)(test|dev)/|_test\\.clj"
   :ignore-ns "^(clojure\\.|cljs\\.core$)"
   :ignore-classes "^java\\.lang\\."
   :io {:postgres "^(next\\.jdbc|hugsql|clojure\\.java\\.jdbc|org\\.postgresql|com\\.zaxxer\\.hikari)"
        :redis "^(taoensso\\.carmine|redis\\.clients)"
        :xtdb "^xtdb"
        :queue "^proletarian"
        :http "^(clj-http|hato|babashka\\.http-client|org\\.httpkit\\.client|martian|java\\.net\\.http)"
        :kafka "^(jackdaw|org\\.apache\\.kafka)"
        :s3 "^(cognitect\\.aws|software\\.amazon)"}
   :generators {"hugsql" "^hugsql\\.core/def-(db|sqlvec)-fns$"}})

(defn- config-for
  "`defaults` merged with the repository's entry in `~/.config/sdiff/deps.edn`, if any."
  [repo]
  (let [f (io/file (System/getProperty "user.home") ".config" "sdiff" "deps.edn")
        c (merge-with #(if (map? %1) (merge %1 %2) %2) defaults (when (.exists f) (get (edn/read-string (slurp f)) repo)))]
    (assoc c :hash (hash c))))

(defonce ^:private bridges (atom []))

(defn add-bridge!
  "`(fn [{:keys [analysis form-at]}] [[from-node to-node] ...])`, edges a tool
  knows that a call graph cannot see, such as a keyword that names a function
  registered elsewhere. Nodes are `[file row]` as `form-at` returns them."
  [k f]
  (swap! bridges (fn [bs] (conj (vec (remove #(= k (first %)) bs)) [k f]))))

(defn- by-position [entries] (sort-by (juxt :row :col) entries))

(defn keyword-bridge
  "A bridge where a form that names a keyword depends on the form that defines
  it. A form defines a keyword when a call matching `defining` (a regex over the
  called var's qualified name) is followed, as its first keyword, by that
  keyword: `(s/def ::k ...)`, or a registration such as `(register! :fn/x ...)`."
  [defining]
  (fn [{:keys [analysis form-at]}]
    (let [kws (update-vals (group-by :filename (filter :ns (:keywords analysis))) by-position)
          kw (fn [{:keys [ns name]}] (keyword (str ns) name))
          first-after (fn [file row col] (some #(when (or (> (:row %) row) (and (= (:row %) row) (> (:col %) col))) %) (kws file)))
          defines (into {} (for [{:keys [filename row col to name]} (:var-usages analysis)
                                 :when (and to (re-find defining (str to "/" name)))
                                 :let [k (first-after filename row col) f (form-at filename row)]
                                 :when (and k f (= f (form-at filename (:row k))))]
                             [(kw k) f]))]
      (for [file (keys kws) e (kws file)
            :let [from (form-at file (:row e)) to (defines (kw e))]
            :when (and from to (not= from to))]
        [from to]))))

(add-bridge! ::spec (keyword-bridge #"^(clojure|cljs)\.spec\.alpha/def$"))

(defn- top-forms [file]
  (try
    (for [node (core/forms (slurp file))
          :let [{:keys [row end-row]} (meta node)] :when row]
      {:file file :row row :end-row end-row :id (vec (remove nil? (core/top-id node)))})
    (catch Exception _ nil)))

(defn- sources [root]
  (for [f (file-seq (io/file root)) :when (and (.isFile f) (re-find #"\.clj[cs]?$" (.getName f)))]
    (.getCanonicalPath f)))

(defn graph
  "The dependency graph of every Clojure source under `root`, as plain data
  keyed by `[path row]` with paths relative to `root`."
  [root config]
  (let [root (.getCanonicalPath (io/file root))
        prefix (str root "/")
        rel (fn [f] (if (str/starts-with? f prefix) (subs f (count prefix)) f))
        {:keys [analysis findings]} (kondo/lint [root] {:output {:canonical-paths true}
                                                        :linters {:unresolved-symbol {:level :warning}}
                                                        :analysis {:keywords true :protocol-impls true :java-class-usages true}})
        a analysis
        tops (vec (mapcat top-forms (sources root)))
        methods (for [{:keys [filename row end-row method-name]} (:protocol-impls a) :when (and row end-row)]
                  {:file filename :row row :end-row end-row :method (str method-name)})
        spans (update-vals (group-by :file (concat tops methods)) #(sort-by (fn [f] (- (:end-row f) (:row f))) %))
        form-at (fn [file row] (when row (some #(when (<= (:row %) row (:end-row %)) [file (:row %)]) (spans file))))
        ns-of (into {} (for [{:keys [filename name]} (:namespace-definitions a)] [filename (str name)]))
        project (set (vals ns-of))
        ignore-ns (re-pattern (:ignore-ns config))
        ignore-class (re-pattern (:ignore-classes config))
        io-kinds (for [[k re] (:io config)] [k (re-pattern re)])
        kinds (fn [s] (set (for [[k re] io-kinds :when (re-find re s)] k)))
        var-def (into {} (for [{:keys [filename row ns name]} (:var-definitions a) :let [f (form-at filename row)] :when f] [[ns name] f]))
        test? (let [re (re-pattern (:test-paths config))] (fn [f] (boolean (re-find re (rel f)))))
        impls (reduce (fn [m {:keys [protocol-ns method-name filename row]}]
                        (if-let [f (form-at filename row)]
                          (if (test? filename) m (update m [protocol-ns method-name] (fnil conj #{}) f))
                          m))
                      {} (:protocol-impls a))
        generated (into {} (for [{:keys [from to name]} (:var-usages a)
                                 [lib re] (:generators config) :when (re-find (re-pattern re) (str to "/" name))]
                             [(str from) lib]))
        add (fn [m k v] (if (and k v) (update m k (fnil conj #{}) v) m))
        usages (for [{:keys [filename row to name]} (:var-usages a) :let [from (form-at filename row)] :when (and from to)]
                 [from (str to) name])
        calls (reduce (fn [m [from to name]]
                        (let [target (var-def [(symbol to) name]) is (impls [(symbol to) name])]
                          (cond-> m
                            (and target (not= target from)) (add from target)
                            (= 1 (count is)) (add from (first is)))))
                      {} usages)
        calls (reduce (fn [m [from to]] (if (not= from to) (add m from to) m))
                      calls
                      (for [[_ f] @bridges e (f {:analysis a :form-at form-at})] e))
        may (reduce (fn [m [from to name]] (let [is (impls [(symbol to) name])] (if (> (count is) 1) (reduce #(add %1 from %2) m is) m)))
                    {} usages)
        ext (as-> {} m
              (reduce (fn [m [from to]] (if (or (project to) (re-find ignore-ns to)) m (add m from to))) m usages)
              (reduce (fn [m {:keys [filename row class]}]
                        (let [from (form-at filename row) pkg (str/replace (str class) #"\.[^.]+$" "")]
                          (if (or (nil? from) (re-find ignore-class (str class))) m (add m from pkg))))
                      m (:java-class-usages a))
              (reduce (fn [m {:keys [filename row]}]
                        (if-let [lib (generated (ns-of filename))] (add m (form-at filename row) (str lib " (generated)")) m))
                      m (filter #(= :unresolved-symbol (:type %)) findings)))
        io (into {} (for [[node libs] ext :let [ks (set (mapcat kinds libs))] :when (seq ks)] [node ks]))
        var-at (into {} (for [{:keys [filename row ns name]} (:var-definitions a) :let [f (form-at filename row)] :when f] [f (str ns "/" name)]))
        forms (into {} (for [f (concat tops methods)
                             :let [k [(:file f) (:row f)]]]
                         [[(rel (:file f)) (:row f)]
                          {:ns (ns-of (:file f))
                           :test (test? (:file f))
                           :var (when-not (:method f) (var-at k))
                           :name (if (:method f)
                                   (str (some #(when (and (not (:method %)) (<= (:row %) (:row f) (:end-row %))) (str/join " " (:id %))) (spans (:file f)))
                                        " · " (:method f))
                                   (str/replace (str/join " " (:id f)) #"\^\S+\s+|\s+" " "))}]))
        relk (fn [[f r]] [(rel f) r])
        relm (fn [m] (into {} (for [[k vs] m] [(relk k) (if (set? vs) (set (map #(if (vector? %) (relk %) %) vs)) vs)])))]
    {:forms forms
     :calls (relm calls) :may (relm may) :ext (relm ext) :io (relm io)
     :ns-deps (reduce (fn [m {:keys [from to]}] (update m (str from) (fnil conj #{}) (str to))) {} (:namespace-usages a))
     :project project}))

(def ^:private cache-root (io/file (System/getProperty "user.home") ".cache" "sdiff"))

(defn- source-dir
  "The repository's Clojure sources at `sha`, fetched once from GitHub with `gh`."
  [repo sha]
  (let [dir (io/file cache-root "src" (str/replace repo "/" "--") sha)]
    (when-not (.exists (io/file dir ".complete"))
      (.mkdirs dir)
      (let [{:keys [exit err]} (sh "bash" "-c" (str "gh api repos/" repo "/tarball/" sha " | tar -xz -C " dir
                                                   " --strip-components=1 --wildcards '*.clj*'"))]
        (when-not (zero? exit) (throw (ex-info (str "could not fetch " repo "@" sha ": " err) {})))
        (spit (io/file dir ".complete") "")))
    (.getCanonicalPath dir)))

(defonce ^:private snapshots (atom {}))

(defn snapshot
  "The dependency graph of `repo` at `sha`, computed once and kept on disk."
  [repo sha]
  (let [config (config-for repo)
        k [repo sha (:hash config) (map first @bridges)]
        f (io/file cache-root "deps" (str (str/replace repo "/" "--") "-" sha "-" (Math/abs (hash k)) ".edn"))]
    (or (@snapshots k)
        (let [g (if (.exists f)
                  (edn/read-string (slurp f))
                  (let [g (graph (source-dir repo sha) config)] (io/make-parents f) (spit f (pr-str g)) g))]
          (swap! snapshots assoc k g)
          g))))

(defn- reverse-of [m] (reduce (fn [r [from tos]] (reduce #(update %1 %2 (fnil conj #{}) from) r tos)) {} m))

(defn- reach [{:keys [calls io]} node]
  (loop [frontier [node] seen #{node} found #{}]
    (if-let [x (peek frontier)]
      (let [nx (remove seen (calls x))]
        (recur (into (pop frontier) nx) (into seen nx) (into found (io x))))
      found)))

(defn- cycles
  "Namespace cycles among `project`: strongly connected groups of two or more."
  [deps project]
  (let [g (into {} (for [[f ts] deps :when (project f)] [f (filter project ts)]))
        idx (atom 0) stack (atom []) on (atom #{}) index (atom {}) low (atom {}) out (atom [])]
    (letfn [(visit [v]
              (swap! index assoc v @idx) (swap! low assoc v @idx) (swap! idx inc)
              (swap! stack conj v) (swap! on conj v)
              (doseq [w (g v)]
                (cond (not (contains? @index w)) (do (visit w) (swap! low update v min (@low w)))
                      (@on w) (swap! low update v min (@index w))))
              (when (= (@low v) (@index v))
                (let [c (loop [c #{}] (let [w (peek @stack)] (swap! stack pop) (swap! on disj w) (if (= w v) (conj c w) (recur (conj c w)))))]
                  (when (> (count c) 1) (swap! out conj c)))))]
      (doseq [v (keys g) :when (not (contains? @index v))] (visit v))
      (set @out))))

(defn- node-of [snap path src id]
  (when (seq src)
    (when-let [row (:row (meta (get (core/index src) id)))]
      (let [k [path row]] (when (get-in snap [:forms k]) k)))))

(defn- names [snap nodes] (set (keep #(get-in snap [:forms % :name]) nodes)))

(defn- defined-keyword [name] (second (re-find #"^\S+ (:\S+)" (str name))))

(defn- label
  "How a form is named on the page: its var, else the keyword it defines, else
  its namespace and head."
  [snap node]
  (let [{:keys [ns name var]} (get-in snap [:forms node])
        [enclosing method] (str/split (str name) #" · " 2)]
    (or var
        (when method
          (str method " in " (or (last (re-find #"^defmethod \S+ (\S+)" enclosing)) (last (str/split enclosing #" ")))))
        (when-not (str/starts-with? (str name) "defmethod") (defined-keyword name))
        (str ns " · " name))))

(defn- qualified [snap node] (label snap node))

(defn form-deps
  "For one changed form: callers at head, and the callees, libraries and I/O
  it has at head against base."
  [{:keys [base head]} file form]
  (let [path (:path file)
        h (node-of head path (:new file) (:id form))
        b (node-of base path (:old file) (or (:was form) (:id form)))
        callers (fn [snap node] (when node (get-in snap [:callers node] #{})))
        callees (fn [snap node] (when node (set (map #(qualified snap %) (get-in snap [:calls node])))))
        ext (fn [snap node] (when node (get-in snap [:ext node] #{})))
        rch (fn [snap node] (when node (reach snap node)))]
    (when (or h b)
      (let [ch (callers head h) cb (callers base b)
            ee-h (callees head h) ee-b (callees base b)
            x-h (ext head h) x-b (ext base b)
            r-h (rch head h) r-b (rch base b)]
        {:removed? (nil? h) :new? (nil? b)
         :callers (sort (map #(qualified head %) ch)) :caller-ns (count (set (keep #(get-in head [:forms % :ns]) ch)))
         :callers-by-ns (into (sorted-map) (update-vals (group-by #(get-in head [:forms % :ns]) ch) (fn [ns] (sort (map #(qualified head %) ns)))))
         :all-reach (sort r-h)
         :test-callers (count (filter #(get-in head [:forms % :test]) ch))
         :outside-callers (count (remove #(= (get-in head [:forms % :ns]) (get-in head [:forms h :ns])) ch))
         :callers-before (count cb)
         :calls-added (sort (remove (or ee-b #{}) ee-h)) :calls-removed (sort (remove (or ee-h #{}) ee-b))
         :libs (sort x-h) :libs-added (sort (remove (or x-b #{}) x-h)) :libs-removed (sort (remove (or x-h #{}) x-b))
         :reaches (sort r-h) :reaches-added (sort (remove (or r-b #{}) r-h)) :reaches-removed (sort (remove (or r-h #{}) r-b))}))))

(defn ns-changes
  "Requires added and removed in the changed namespaces, and namespace cycles
  that exist at head and not at base."
  [{:keys [base head]} changed-ns]
  (let [proj (:project head)
        ignore (re-pattern (:ignore-ns defaults))
        keep-ns (fn [xs] (set (remove #(re-find ignore %) xs)))
        reqs (for [ns (sort changed-ns)
                   :let [hb (keep-ns (get-in base [:ns-deps ns])) hh (keep-ns (get-in head [:ns-deps ns]))
                         added (sort (remove hb hh)) removed (sort (remove hh hb))]
                   :when (or (seq added) (seq removed))]
               {:ns ns :added added :removed removed :internal-added (filter proj added)})
        new-cycles (remove (cycles (:ns-deps base) (:project base)) (cycles (:ns-deps head) proj))]
    {:requires (vec reqs) :cycles (vec (filter #(some changed-ns %) new-cycles))}))

(defn- enabled? [report]
  (and (get-in report [:pr :repo]) (not (:skip-deps (state/settings)))))

(defonce ^:private per-report (atom {}))

(defn data
  "Base and head snapshots for a report, or `{:error msg}`."
  [report]
  (when (enabled? report)
    (let [{:keys [repo]} (:pr report) k [repo (:base report) (:head report)]]
      (or (@per-report k)
          (let [with-callers (fn [g] (assoc g :callers (merge-with into (reverse-of (:calls g)) (reverse-of (:may g)))))
                d (try {:base (with-callers (snapshot repo (:base report))) :head (with-callers (snapshot repo (:head report)))}
                       (catch Exception e {:error (ex-message e)}))]
            (swap! per-report assoc k d)
            d)))))

(defn- tag [cls x & [t]] [:span.tag {:class cls :title t} (str x)])

(defn- tags [cls xs] (interpose " " (for [x xs] (tag cls (if (keyword? x) (name x) x)))))

(defn- per-form [d report]
  (for [f (:clj report) :when (= :semantic (:verdict f)) form (:forms f) :let [x (form-deps d f form)] :when x] [f form x]))

(defn- couplings
  "Project namespaces outside tests that start requiring another project
  namespace: new coupling between parts of the code."
  [d requires]
  (let [test-ns (set (keep (fn [[_ v]] (when (:test v) (:ns v))) (get-in d [:head :forms])))]
    (for [{:keys [ns internal-added]} requires :when (not (test-ns ns)) to internal-added :when (not (test-ns to))] [ns to])))

(defn header [ctx report]
  (when-let [d (data report)]
    (if (:error d)
      (derived "deps" [:p "Dependency analysis failed: " (:error d)])
      (let [forms (per-form d report)
            ns-of-path (into {} (for [[[p] v] (concat (get-in d [:base :forms]) (get-in d [:head :forms])) :when (:ns v)] [p (:ns v)]))
            changed-ns (set (keep #(ns-of-path (:path %)) (:clj report)))
            {:keys [requires cycles]} (ns-changes d changed-ns)
            code? (let [re (re-pattern (:test-paths defaults))] (fn [[f]] (not (re-find re (:path f)))))
            new-io (filter (fn [[_ _ x :as e]] (and (code? e) (seq (:reaches-added x)))) forms)
            coupled (couplings d requires)]
        (derived "deps · clj-kondo over the whole repository, base and head; the lines under each form come from the same analysis"
                 [:h5 "Dependencies"]
                 [:p (count forms) " changed forms · " (count new-io) " outside tests reach a new kind of I/O · "
                  (count coupled) " new dependencies between project namespaces · " (count cycles) " new namespace cycles"]
                 (when (seq new-io)
                   [:ul (for [[_ form x] new-io] [:li [:code (or (defined-keyword (decorate/form-id form)) (decorate/form-id form))] " now reaches " (tags "tag-ext" (:reaches-added x))])])
                 (when (seq coupled)
                   [:ul (for [[from to] coupled] [:li [:code from] " now requires " [:code to]])])
                 (when (seq cycles)
                   [:p "New cycles: " (interpose "; " (for [c cycles] (str/join " ↔ " (sort c))))]))))))

(defn by-callers
  "Changed forms banded by how many forms outside their own namespace call them,
  widest first: where a change reaches furthest is where to read first."
  [_ctx report]
  (when-let [d (data report)]
    (when-not (:error d)
      (let [rows (for [[f form x] (per-form d report)] [[(:path f) (decorate/form-id form)] x])
            test? (let [re (re-pattern (:test-paths defaults))] (fn [p] (re-find re p)))
            band (fn [[[p] x]] (let [n (:outside-callers x 0)]
                                 (cond (:removed? x) (if (pos? (:callers-before x)) 4 5)
                                       (test? p) 6
                                       (>= n 10) 0 (>= n 2) 1 (= n 1) 2 :else 3)))
            titles ["Called from 10 or more forms in other namespaces" "Called from 2 to 9 forms in other namespaces"
                    "Called from one form in another namespace" "Called only within its own namespace, or not at all"
                    "Removed, and had callers" "Removed" "In tests"]]
        (for [[b rs] (sort-by key (group-by band rows))]
          {:title (titles b)
           :claim (str (count rs) " form" (when (not= 1 (count rs)) "s") ", most callers first.")
           :items (vec (for [[[p id]] (sort-by (fn [[_ x]] [(- (:outside-callers x 0)) (- (count (:callers x)))]) rs)] [:form p id]))})))))

(defn- codes [xs] (interpose ", " (map (fn [c] [:code c]) xs)))

(defn- line-parts
  "What is worth saying about one changed form: callers for any form, and for a
  form that existed before, the calls, libraries and I/O it gained or lost."
  [x]
  (let [existing? (not (or (:new? x) (:removed? x)))
        callers-worth-saying? (or (pos? (:outside-callers x 0))
                                  (and existing? (not= (count (:callers x)) (:callers-before x))))]
    (remove nil?
            [(cond (:removed? x) (when (pos? (:callers-before x)) (str "removed · had " (:callers-before x) " callers"))
                   (and (seq (:callers x)) callers-worth-saying?) (str "← " (count (:callers x)) " caller" (when (not= 1 (count (:callers x))) "s")
                                           " · " (:caller-ns x) " ns"
                                           (when (pos? (:test-callers x)) (str " · " (:test-callers x) " in tests"))
                                           (when (and existing? (not= (count (:callers x)) (:callers-before x))) (str " (was " (:callers-before x) ")"))))
             (when (and existing? (seq (:calls-added x))) (list "→ now " (codes (:calls-added x))))
             (when (and existing? (seq (:calls-removed x))) (list "no longer " (codes (:calls-removed x))))
             (when (and existing? (seq (:libs-added x))) (list "new libraries " (tags "tag-ext" (:libs-added x))))
             (when (and existing? (seq (:libs-removed x))) (list "drops " (tags "tag-del" (:libs-removed x))))
             (when (and (not (:removed? x)) (seq (:reaches-added x))) (list "now reaches " (tags "tag-ext" (:reaches-added x))))
             (when (seq (:reaches-removed x)) (list "no longer reaches " (tags "tag-del" (:reaches-removed x))))])))

(defn form-line [ctx file form]
  (let [report (:report ctx)]
    (when-let [d (data report)]
      (when-not (:error d)
        (let [file (or (some #(when (= (:path file) (:path %)) %) (:clj report)) file)
              full (or (some #(when (= (:id form) (:id %)) %) (:forms file)) form)
              test-file? (re-find (re-pattern (:test-paths defaults)) (str (:path file)))
              x (when-not test-file? (form-deps d file full))
              parts (when x (line-parts x))]
          (when (seq parts)
            [:div.deps-line {:title (when (seq (:all-reach x)) (str "reaches " (str/join ", " (map name (:all-reach x)))))}
             (interpose [:span.sep " · "] parts)
             (when (seq (:callers x))
               [:details.deps-callers [:summary "callers"]
                [:ul (for [[ns cs] (:callers-by-ns x)] [:li [:code.mute ns] " " (codes cs)])]])]))))))

(decorate/add-header-decorator! ::deps header)
(decorate/add-grouper! :callers "callers" "deps · clj-kondo over the whole repository" by-callers)
(decorate/add-form-decorator! ::deps form-line)
