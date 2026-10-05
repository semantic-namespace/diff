(ns sdiff.git
  "Git plumbing: the changed files of a range and their blobs on each side,
  assembled into a report of structural file reports."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [sdiff.core :as core]
            [sdiff.hunks :as hunks]))

(defn git [repo & args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" repo args)]
    (if (zero? exit) out (throw (ex-info err {:args args})))))

(defn merge-base [repo base head] (str/trim (git repo "merge-base" base head)))

(defn changed-files [repo base head]
  (for [l (str/split-lines (git repo "diff" "--name-status" base head))
        :let [[st & ps] (str/split l #"\t")] :when (seq ps)]
    {:status (subs st 0 1) :path (last ps) :old-path (first ps)}))

(defn blob [repo rev path] (try (git repo "show" (str rev ":" path)) (catch Exception _ "")))

(defn report
  "Structural report of the range base..head in repo:
  `{:base :head :clj [file-report...] :renames [...] :other [non-clojure files]}`,
  each file report with the head-side `:hunks` of its diff.
  Pass `:merge-base? true` to diff from the merge base of the two refs, which is
  what a pull request shows; diffing from the base branch tip would include
  unrelated drift from the base."
  [repo base head & {:keys [merge-base?]}]
  (let [base (if merge-base? (merge-base repo base head) base)
        files (changed-files repo base head)
        {:keys [files renames]} (core/rollup-renames
                                 (vec (for [{:keys [path old-path status]} files :when (core/clj? path)]
                                        (assoc (core/file-report path (blob repo base old-path) (blob repo head path))
                                               :status status
                                               :hunks (hunks/right-ranges (git repo "diff" "-U3" base head "--" path))))))]
    {:base base :head head :clj files :renames renames
     :other (vec (remove #(core/clj? (:path %)) files))}))
