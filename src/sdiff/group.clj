(ns sdiff.group
  "Groupings of a report's changed forms, each a view whose sections are the
  groups. A grouping is derived: it comes from the code, never from a reading
  of it. Built in are grouping by directory and by the calls between changed
  forms; other tools add their own with `sdiff.decorate/add-grouper!`."
  (:require [clojure.string :as str]
            [sdiff.core :as core]
            [sdiff.decorate :as decorate]
            [sdiff.kondo :as kondo]))

(defonce ^:private cache (atom {}))

(defn analysis
  "clj-kondo's analysis of the head side of every changed Clojure file."
  [report]
  (let [k [(:head report) (mapv :path (:clj report))]]
    (or (@cache k)
        (let [a (kondo/analyse (into {} (for [f (:clj report) :when (seq (:new f))] [(:path f) (:new f)])))]
          (swap! cache assoc k a)
          a))))

(defn nodes
  "`[path form-id]` of every changed form in a file that changes behaviour."
  [report]
  (vec (for [f (:clj report) :when (= :semantic (:verdict f)) form (:forms f)]
         [(:path f) (decorate/form-id form)])))

(defn- spans [report]
  (into {} (for [f (:clj report) :when (and (= :semantic (:verdict f)) (seq (:new f)))
                 :let [idx (core/index (:new f))]]
             [(:path f) (vec (for [form (:forms f)
                                   :let [{:keys [row end-row]} (meta (get idx (:id form)))] :when row]
                               [row end-row [(:path f) (decorate/form-id form)]]))])))

(defn form-at
  "The changed form of `report` that holds `row` of `path`, as `[path form-id]`."
  [report]
  (let [s (spans report)]
    (fn [path row] (some (fn [[r e k]] (when (<= r row e) k)) (get s path)))))

(defn call-edges
  "Pairs of changed forms where one calls a var the other defines."
  [report]
  (let [a (analysis report) at (form-at report)
        defs (into {} (for [{:keys [filename row ns name]} (:var-definitions a) :let [k (at filename row)] :when k] [[ns name] k]))]
    (set (for [{:keys [filename row to name]} (:var-usages a)
               :let [from (at filename row) to-form (defs [to name])]
               :when (and from to-form (not= from to-form))]
           #{from to-form}))))

(defn keyword-mentions
  "`{[path form-id] #{keyword}}`: the qualified keywords each changed form names."
  [report]
  (let [a (analysis report) at (form-at report)]
    (reduce (fn [m {:keys [filename row ns name]}]
              (if-let [k (and ns (at filename row))] (update m k (fnil conj #{}) (keyword (str ns) name)) m))
            {} (:keywords a))))

(defn components
  "Connected components of `nodes` under `edges` (sets of two nodes), largest first."
  [nodes edges]
  (let [adj (reduce (fn [m e] (let [[a b] (seq e)] (-> m (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))) {} edges)]
    (loop [left (vec nodes) seen #{} out []]
      (if-let [n (first left)]
        (if (seen n)
          (recur (rest left) seen out)
          (let [c (loop [q [n] c #{n}]
                    (if-let [x (first q)]
                      (let [nx (remove c (adj x))] (recur (into (vec (rest q)) nx) (into c nx)))
                      c))]
            (recur (rest left) (into seen c) (conj out (filterv c nodes)))))
        (sort-by (comp - count) out)))))

(defn section [title claim members]
  {:title title :claim claim :items (vec (for [[p id] members] [:form p id]))})

(defn- plural [n w] (str n " " w (when (not= 1 n) "s")))

(defn- hub [members edges]
  (first (sort-by (fn [m] [(- (count (filter #(contains? % m) edges))) (str m)]) members)))

(defn by-calls [_ctx report]
  (let [ns (nodes report) es (call-edges report)
        {groups true singles false} (group-by #(> (count %) 1) (components ns es))]
    (concat
     (for [g groups :let [[p id] (hub g es)]]
       (section (str "Around " id) (str (plural (count g) "form") " that call one another, in " (plural (count (distinct (map first g))) "file") ".") g))
     (when (seq singles)
       [(section "Changes that call no other changed form" nil (apply concat singles))]))))

(defn by-directory [_ctx report]
  (for [[dir g] (sort-by key (group-by #(let [p (first %)] (subs p 0 (max 0 (or (str/last-index-of p "/") 0)))) (nodes report)))]
    (section (if (str/blank? dir) "top level" dir) (str (plural (count g) "form") " in " (plural (count (distinct (map first g))) "file") ".") g)))

(defn outline-text
  "The groups of a grouping view as plain text, one line per form, ahead of the report."
  [{:keys [title derived sections]}]
  (str title " (derived · " derived ")\n"
       (apply str (for [[i {:keys [title claim items]}] (map-indexed vector sections)]
                    (str (inc i) ". " title (when claim (str " — " claim)) "\n"
                         (apply str (for [[_ p id] items] (str "   " p " · " id "\n"))))))
       "\n"))

(decorate/add-grouper! :calls "calls between forms" "sdiff · clj-kondo calls" by-calls)
(decorate/add-grouper! :directory "directory" "sdiff · paths" by-directory)
