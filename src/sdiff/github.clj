(ns sdiff.github
  "A pull request on GitHub as a source, read through the `gh` CLI so every call
  runs with the caller's own token and grants. Nothing is cloned: blobs come
  from the contents API at the merge base and the head commit, which is what
  the PR's Files tab shows."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [cheshire.core :as json]
            [sdiff.core :as core]
            [sdiff.hunks :as hunks]))

(defn parse-pr
  "`owner/repo#N`, `owner/repo N` or a pull-request URL, as `{:repo \"owner/repo\" :num N}`."
  [s]
  (let [s (str/trim (str s))]
    (if-let [[_ repo n] (or (re-find #"github\.com/([^/]+/[^/]+)/pull/(\d+)" s)
                            (re-find #"^([\w.-]+/[\w.-]+)[#\s]+(\d+)$" s))]
      {:repo repo :num (parse-long n)}
      (throw (ex-info (str "not a pull request reference: " s " (use owner/repo#N or a PR URL)") {:pr s})))))

(defn gh
  "Run `gh` with args; returns stdout, or throws with gh's message."
  [args & {:keys [in]}]
  (let [{:keys [exit out err]} (apply sh "gh" (concat args (when in [:in in])))]
    (if (zero? exit) out (throw (ex-info (str/trim (str err out)) {:args args})))))

(defn api-json [path & args] (json/parse-string (gh (concat ["api" path] args)) true))

(defn- encode-path [p] (str/join "/" (map #(java.net.URLEncoder/encode ^String % "UTF-8") (str/split p #"/"))))

(defn blob
  "File content at `ref`, or \"\" when the file does not exist there."
  [repo ref path]
  (try (gh ["api" "-H" "Accept: application/vnd.github.raw" (str "repos/" repo "/contents/" (encode-path path) "?ref=" ref)])
       (catch Exception _ "")))

(defn pr-info [{:keys [repo num]}]
  (let [p (api-json (str "repos/" repo "/pulls/" num))
        base (get-in p [:base :sha]) head (get-in p [:head :sha])
        mb (:sha (:merge_base_commit (api-json (str "repos/" repo "/compare/" base "..." head))))]
    {:repo repo :num num :title (:title p) :url (:html_url p) :author (get-in p [:user :login])
     :base mb :head head :state (:state p)}))

(defn pr-files [{:keys [repo num]}]
  (let [out (gh ["api" "--paginate" (str "repos/" repo "/pulls/" num "/files?per_page=100")
                 "--jq" ".[] | {filename, status, previous_filename, patch}"])]
    (for [l (str/split-lines out) :when (not (str/blank? l))] (json/parse-string l true))))

(def ^:private status {"added" "A" "removed" "D" "renamed" "R" "modified" "M" "changed" "M" "copied" "C"})

(defn report
  "Structural report of a pull request, in the shape of `sdiff.git/report`, plus
  `:pr` (number, title, url, author) and per-file `:hunks`. A file whose patch
  GitHub did not send (too large) gets no hunks, so every note on it goes to
  the review body."
  [pr-ref]
  (let [pr (pr-info (if (map? pr-ref) pr-ref (parse-pr pr-ref)))
        pr-fs (pr-files pr)
        clj (for [{:keys [filename previous_filename patch] st :status} pr-fs :when (core/clj? filename)]
              (assoc (core/file-report filename
                                       (blob (:repo pr) (:base pr) (or previous_filename filename))
                                       (blob (:repo pr) (:head pr) filename))
                     :status (status st "M")
                     :hunks (hunks/right-ranges patch)))
        {:keys [files renames]} (core/rollup-renames (vec clj))]
    {:base (:base pr) :head (:head pr) :pr pr :num (:num pr) :title (:title pr)
     :clj files :renames renames
     :other (vec (for [{:keys [filename] st :status} pr-fs :when (not (core/clj? filename))]
                   {:status (status st "M") :path filename}))}))

(defonce ^:private cache (atom {}))

(defn cached-report
  "`report` for a PR reference, fetched once per head commit, so a draft and the
  post that follows it anchor notes to the same lines."
  [pr-ref]
  (let [info (pr-info (if (map? pr-ref) pr-ref (parse-pr pr-ref)))
        k [(:repo info) (:num info) (:head info)]]
    (or (@cache k)
        (let [r (report info)] (swap! cache assoc k r) r))))

(def ^:private viewed-query
  "query($owner:String!,$name:String!,$number:Int!,$endCursor:String){repository(owner:$owner,name:$name){pullRequest(number:$number){id files(first:100,after:$endCursor){pageInfo{hasNextPage endCursor} nodes{path viewerViewedState}}}}}")

(defn viewed
  "The caller's per-file Viewed state on a PR, as GitHub keeps it:
  `{:pr-id id :files {path \"VIEWED\"|\"UNVIEWED\"|\"DISMISSED\"}}`. DISMISSED means
  the file changed after it was marked viewed."
  [pr-ref]
  (let [{:keys [repo num]} (if (map? pr-ref) pr-ref (parse-pr pr-ref))
        [owner name] (str/split repo #"/")
        out (gh ["api" "graphql" "--paginate" "-F" (str "owner=" owner) "-F" (str "name=" name) "-F" (str "number=" num)
                 "-f" (str "query=" viewed-query)
                 "--jq" ".data.repository.pullRequest | {id, files: .files.nodes}"])
        pages (for [l (str/split-lines out) :when (not (str/blank? l))] (json/parse-string l true))]
    {:pr-id (:id (first pages))
     :files (into {} (for [p pages f (:files p)] [(:path f) (:viewerViewedState f)]))}))

(defn set-viewed!
  "Mark or unmark `path` as viewed on the PR with GraphQL node id `pr-id`, as the caller."
  [pr-id path viewed?]
  (let [m (if viewed? "markFileAsViewed" "unmarkFileAsViewed")]
    (gh ["api" "graphql" "-f" (str "query=mutation($id:ID!,$path:String!){" m "(input:{pullRequestId:$id,path:$path}){clientMutationId}}")
         "-f" (str "id=" pr-id) "-f" (str "path=" path)])
    (if viewed? "VIEWED" "UNVIEWED")))
