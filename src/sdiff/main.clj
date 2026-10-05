(ns sdiff.main
  "Command line.

    text   <repo> <base> <head>
    edn    <repo> <base> <head>
    html   <repo> <out.html> <gh-url|-> (<base> <head> <num> <title>)+
    pr     <owner/repo#N|url> [text|edn|html <out.html>]
    review <owner/repo#N|url> <notes.edn> [--post]
    serve  [port] [--host addr]        review UI (default 127.0.0.1:7878); any other
                                       host requires the printed access link

  Local ranges are diffed from the merge base of base and head, which is what a
  pull request shows. For a squash-merged commit C, use `C^ C`. `pr` and
  `review` read GitHub through the gh CLI with your own credentials.

  notes.edn is `{:verdict :approve|:request-changes|:comment :summary \"…\"
  :notes [{:file … :form \"defn x\" :at \"body › let 1\" :body \"…\"}]}`.
  `review` prints the GitHub payload and where each note lands; with `--post`
  it then asks before posting."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sdiff.edn :as sedn]
            [sdiff.git :as git]
            [sdiff.github :as github]
            [sdiff.render.html :as html]
            [sdiff.render.text :as text]
            [sdiff.review :as review]
            [sdiff.serve :as serve]))

(defn- repo-name [repo gh-url]
  (if gh-url
    (last (str/split gh-url #"github.com/"))
    (last (str/split (.getCanonicalPath (java.io.File. ^String repo)) #"/"))))

(defn- print-placement [placed]
  (doseq [{:keys [note inline body]} placed]
    (println (str (if inline (str "inline  " (:path inline) ":" (or (:start_line inline) (:line inline))
                                  (when (:start_line inline) (str "-" (:line inline))))
                      (str "body    (" body ")"))
                  "  ← " (or (:form note) (str "line " (:line note))) (when (:at note) (str " › " (:at note)))))))

(defn- review! [pr notes-file post?]
  (let [{:keys [verdict summary notes]} (edn/read-string (slurp notes-file))
        r (github/report pr)
        {:keys [payload placed]} (review/draft r verdict summary notes)
        {:keys [repo num]} (:pr r)]
    (print-placement placed)
    (println)
    (println (json/generate-string payload {:pretty true}))
    (when post?
      (print (str "\nPost this " (:event payload) " review to " repo "#" num " as your GitHub user? [y/N] "))
      (flush)
      (if (= "y" (str/lower-case (str/trim (or (read-line) ""))))
        (let [res (json/parse-string (github/gh ["api" "-X" "POST" (str "repos/" repo "/pulls/" num "/reviews") "--input" "-"]
                                                :in (json/generate-string payload))
                                     true)]
          (println "posted" (:state res) (:html_url res)))
        (println "not posted")))))

(defn -main [& [cmd & args]]
  (case cmd
    "text" (let [[repo base head] args]
             (text/print-report (git/report repo base head :merge-base? true)))
    "edn"  (let [[repo base head] args]
             (prn (sedn/report->edn (git/report repo base head :merge-base? true))))
    "html" (let [[repo out gh-url & quads] args
                 gh-url (when-not (= "-" gh-url) gh-url)
                 prs (for [[base head num title] (partition 4 quads)]
                       (assoc (git/report repo base head :merge-base? true) :num num :title title))]
             (spit out (html/page gh-url (repo-name repo gh-url) prs))
             (println "wrote" out))
    "pr"   (let [[ref fmt out] args
                 r (github/report ref)]
             (case (or fmt "text")
               "text" (do (println (str "#" (:num r) " " (:title r) "\n" (get-in r [:pr :url])))
                          (text/print-report r))
               "edn"  (prn (sedn/report->edn r))
               "html" (do (spit out (html/page (str "https://github.com/" (get-in r [:pr :repo])) (get-in r [:pr :repo]) [r]))
                          (println "wrote" out))))
    "review" (let [[ref notes-file flag] args] (review! ref notes-file (= "--post" flag)))
    "serve" (let [host (second (drop-while #(not= "--host" %) args))
                  port (parse-long (or (first (remove #{"--host" host} args)) "7878"))
                  {:keys [url]} (serve/start! port (or host "127.0.0.1"))]
              (println (str "sdiff review UI on " url "  (Ctrl-C to stop)"))
              (when-not (serve/loopback? (or host "127.0.0.1"))
                (println "Anyone with this link can read PRs and post reviews as your GitHub user. Share it only with yourself."))
              @(promise))
    (do (println (:doc (meta (find-ns 'sdiff.main))))
        (System/exit 2))))
