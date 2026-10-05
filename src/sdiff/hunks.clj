(ns sdiff.hunks
  "Head-side line ranges of a unified diff. GitHub accepts an inline review
  comment only on a line inside one of these ranges, context lines included.")


(defn right-ranges
  "`[[start end] ...]` of the new side of every hunk in a unified `patch`, in
  1-based inclusive lines. A hunk that only deletes has no new-side lines and
  contributes nothing."
  [patch]
  (vec (for [[_ start len] (re-seq #"(?m)^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@" (or patch ""))
             :let [s (parse-long start) n (if len (parse-long len) 1)]
             :when (pos? n)]
         [s (+ s n -1)])))

(defn clamp
  "The part of `[from to]` that falls inside the first hunk it overlaps, or nil."
  [ranges [from to]]
  (some (fn [[s e]] (when (and (<= from e) (<= s to)) [(max from s) (min to e)])) ranges))
