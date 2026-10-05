(ns sdiff.main
  "Command line.

    text <repo> <base> <head>
    edn  <repo> <base> <head>
    html <repo> <out.html> <gh-url|-> (<base> <head> <num> <title>)+

  Ranges are diffed from the merge base of base and head, which is what a pull
  request shows. For a squash-merged commit C, use `C^ C`."
  (:require [clojure.string :as str]
            [sdiff.git :as git]
            [sdiff.edn :as edn]
            [sdiff.render.text :as text]
            [sdiff.render.html :as html]))

(defn- repo-name [repo gh-url]
  (if gh-url
    (last (str/split gh-url #"github.com/"))
    (last (str/split (.getCanonicalPath (java.io.File. ^String repo)) #"/"))))

(defn -main [& [cmd & args]]
  (case cmd
    "text" (let [[repo base head] args]
             (text/print-report (git/report repo base head :merge-base? true)))
    "edn"  (let [[repo base head] args]
             (prn (edn/report->edn (git/report repo base head :merge-base? true))))
    "html" (let [[repo out gh-url & quads] args
                 gh-url (when-not (= "-" gh-url) gh-url)
                 prs (for [[base head num title] (partition 4 quads)]
                       (assoc (git/report repo base head :merge-base? true) :num num :title title))]
             (spit out (html/page gh-url (repo-name repo gh-url) prs))
             (println "wrote" out))
    (do (println (:doc (meta (find-ns 'sdiff.main))))
        (System/exit 2))))
