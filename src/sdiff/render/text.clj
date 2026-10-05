(ns sdiff.render.text
  "Plain-text rendering of a report, one block per file."
  (:refer-clojure :exclude [short])
  (:require [rewrite-clj.node :as n]
            [clojure.string :as str]
            [sdiff.core :refer [fmt-path head short]]
            [sdiff.decorate :as decorate]))

(defn- kept-list [kept] (str/join ", " (map #(short (n/string (first %))) kept)))

(defn print-file [{:keys [path verdict forms status moved]}]
  (println (str "\n■ " path "   [" (name verdict) (case status "A" ", new file" "D" ", deleted" "") "]"))
  (doseq [id moved] (println (str "  " (str/join " " (map str id)) "   ↕ moved to a different position among the forms")))
  (doseq [{:keys [id was changes extraction note]} forms]
    (println (str "  " (str/join " " (map str id))
                  (when was (str "   ⇠ was " (str/join " " (map str was))))
                  (when note (str "   ⚠ " note))
                  (when extraction (str "   ⇠ extracted from " (fmt-path (cons (str/join " " (map str (:from extraction))) (:from-path extraction)))))))
    (doseq [{:keys [op path old new extracted rename] :as c} changes]
      (let [p (if (seq path) (str (fmt-path path) " ") "")]
        (println (case op
                   :added-form   "    + new form"
                   :removed-form "    - form removed"
                   :wrapper      (str "    ~ wrapper " (name (n/tag old)) " → " (name (n/tag new)))
                   :comments     (str "    # " p "comments/docstring only")
                   :visibility   (str "    ~ visibility " (:from c) " → " (:to c))
                   :wrapped      (str "    ~ now wrapped in " (or (head new) (name (n/tag new))) "; the old form is inside it unchanged")
                   :added        (str "    + " p "= " (short (n/string new))
                                      (when-let [f (:moved-from c)] (str "\n        ⇠ holds " (kept-list (:kept c)) " from " (fmt-path f))))
                   :removed      (str "    - " p "= " (short (n/string old)))
                   :reshaped     (str "    ~ " p "restructured " (or (head old) (name (n/tag old))) " → " (or (head new) (name (n/tag new)))
                                      "; kept: " (kept-list (:kept c))
                                      (when extracted (str " [extracted → " extracted "]")))
                   :replaced     (if rename
                                   (str "    ≈ " p "(rename)")
                                   (str "    ~ " p (when extracted (str " [extracted → " extracted "]"))
                                        "\n        - " (short (n/string old)) "\n        + " (short (n/string new))
                                        (when-let [t (:moved-to c)] (str "\n        ⇢ " (kept-list (:kept c)) " moved to " (fmt-path t)))))))))
    (when decorate/*ctx*
      (doseq [t (decorate/form-text {:path path} {:id id})]
        (println (str/join "\n" (map #(str "      " %) (str/split-lines t))))))
    (when extraction
      (when (seq (:renamed extraction))
        (println (str "      renamed locals: " (str/join ", " (map (fn [[o nw]] (str o " → " nw)) (:renamed extraction))))))
      (doseq [{:keys [op path old new]} (:drift extraction)]
        (println (str "      " (if (= op :dropped) "not carried over, from around the extracted part:" (str "drift: " (name op) " " (fmt-path path)))
                      (when old (str "  - " (short (n/string old)))) (when new (str "  + " (short (n/string new))))))))))

(defn print-report [{:keys [clj renames other] :as r}]
  (doseq [{:keys [from to count]} renames] (println (str "≈ rename " from " → " to "  (" count " sites)")))
  (when decorate/*ctx*
    (doseq [t (decorate/header-text r)] (println t) (println)))
  (doseq [fr clj] (print-file fr))
  (when (seq other)
    (println (str "\n" (count other) " non-Clojure file" (when (not= 1 (count other)) "s") ": " (str/join ", " (map :path other))))))
