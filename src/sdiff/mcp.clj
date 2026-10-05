(ns sdiff.mcp
  "MCP server over stdio (JVM; plumcp does not run on babashka). Three tools:

    structural-diff  the report for a pull request or a local git range
    review-draft     notes on form paths -> the GitHub review payload, nothing sent
    post-review      the same payload, posted as the user (write tool)

  Every GitHub call goes through the `gh` CLI, so the server reads and posts with
  exactly the grants of whoever runs it. Reports are cached per PR head commit,
  so a draft and its post see the same anchors."
  (:require [cheshire.core :as json]
            [clojure.edn]
            [clojure.string :as str]
            [plumcp.core.api.entity-gen :as eg]
            [plumcp.core.api.entity-support :as es]
            [plumcp.core.api.mcp-server :as ms]
            [plumcp.core.support.traffic-logger :as stl]
            [sdiff.edn :as sedn]
            [sdiff.git :as git]
            [sdiff.github :as github]
            [sdiff.render.text :as text]
            [sdiff.review :as review]
            [babashka.http-client :as http]
            [sdiff.state :as state]))

(defn pr-report [pr] (github/cached-report pr))

(defn- text-result [s] (eg/make-call-tool-result [(eg/make-text-content s)]))

(defn report-text [{:keys [pr base head] :as r}]
  (str (when pr (str "#" (:num pr) " " (:title pr) "\n" (:url pr) "\n"))
       "range " (subs base 0 (min 12 (count base))) ".." (subs head 0 (min 12 (count head))) "\n"
       (with-out-str (text/print-report r))))

(defn- normalise-notes [notes]
  (mapv (fn [n] (let [n (update-keys n keyword)]
                  (cond-> n (:start_line n) (assoc :start-line (:start_line n)))))
        (if (string? notes) (json/parse-string notes true) notes)))

(defn- placement-text [placed]
  (str/join "\n" (for [{:keys [note inline body]} placed]
                   (str (if inline (str "inline " (:path inline) ":" (or (:start_line inline) (:line inline))
                                        (when (:start_line inline) (str "-" (:line inline))))
                            (str "review body (" body ")"))
                        "  ← " (:file note) " " (or (:form note) (str "line " (:line note)))
                        (when (:at note) (str " › " (:at note)))))))

(defn- ui-server []
  (let [f (java.io.File. (state/dir) "server.edn")]
    (if (.exists f)
      (clojure.edn/read-string (slurp f))
      (throw (ex-info "no review server is running: start `bb sdiff serve` or the atlas review server" {})))))

(defn ^{:mcp-type :tool
        :mcp-annotations (eg/make-tool-annotations :title "Structural diff" :read-only-hint? true :open-world-hint? true)}
  structural-diff
  "Structural report of Clojure changes, named by form and path rather than line.
  Give `pr` (owner/repo#N or a pull-request URL) to read a GitHub PR with the
  caller's gh credentials, or `repo` + `base` + `head` for a local git range
  (diffed from their merge base). `format` is text (default, compact) or edn
  (full: every form's old and new source with line ranges). Each file has a
  verdict; inside semantic files each change is `<form> › <path>`, the names
  review notes refer to. With a review server running, a PR's report comes
  from it and carries the derived decorations and annotations the page shows."
  [{:keys [^{:doc "Pull request: owner/repo#N or https://github.com/owner/repo/pull/N" :type "string" :required? false} pr
           ^{:doc "Local git repository path (instead of pr)" :type "string" :required? false} repo
           ^{:doc "Base ref, with repo" :type "string" :required? false} base
           ^{:doc "Head ref, with repo" :type "string" :required? false} head
           ^{:doc "text or edn" :type "string" :required? false :default "text"} format]}]
  (let [server (try (ui-server) (catch Exception _ nil))]
    (if (and pr server (not= "edn" format))
      (let [{:keys [status body]} (http/get (str (:url server) "pr?ref=" (java.net.URLEncoder/encode pr "UTF-8") "&format=text") {:throw false :timeout 300000})]
        (when (not= 200 status) (throw (ex-info (str "review server: " status " " body) {})))
        (text-result body))
      (let [r (cond pr (pr-report pr)
                    (and repo base head) (git/report repo base head :merge-base? true)
                    :else (throw (ex-info "give pr, or repo with base and head" {})))]
        (text-result (if (= "edn" format) (pr-str (sedn/report->edn r)) (report-text r)))))))

(defn ^{:mcp-type :tool
        :mcp-annotations (eg/make-tool-annotations :title "Draft a review" :read-only-hint? true :open-world-hint? true)}
  review-draft
  "Turn review notes into the GitHub review that post-review would send, without
  sending anything. Shows where each note lands: inline on a head-side line, or
  in the review body when GitHub would refuse it inline (outside the diff, or on
  a removed form). Use this, show the user the result, and only then post."
  [{:keys [^{:doc "Pull request: owner/repo#N or URL" :type "string"} pr
           ^{:doc "approve, request-changes or comment" :type "string"} verdict
           ^{:doc "Review body (markdown)" :type "string" :required? false} summary
           ^{:doc "JSON array of notes. Each: {file, form, at?, body}, where form is a top-level form as structural-diff prints it (defn compile!) and at is a change path inside it (arity 1 › let 2 › binding x); or {file, line, start_line?, body}." :type "array" :required? false} notes]}]
  (let [{:keys [payload placed]} (review/draft (pr-report pr) verdict summary (normalise-notes notes))]
    (text-result (str (placement-text placed) "\n\npayload:\n" (json/generate-string payload {:pretty true})))))

(defn ^{:mcp-type :tool
        :mcp-annotations (eg/make-tool-annotations :title "Post review to GitHub" :read-only-hint? false
                                                   :destructive-hint? true :idempotent-hint? false :open-world-hint? true)}
  post-review
  "Post a review on GitHub as the user: approve, request changes or comment, with
  inline notes. Only call after the user has seen the review-draft output for
  the same arguments and agreed to post it."
  [{:keys [^{:doc "Pull request: owner/repo#N or URL" :type "string"} pr
           ^{:doc "approve, request-changes or comment" :type "string"} verdict
           ^{:doc "Review body (markdown)" :type "string" :required? false} summary
           ^{:doc "JSON array of notes. Each: {file, form, at?, body}, where form is a top-level form as structural-diff prints it (defn compile!) and at is a change path inside it (arity 1 › let 2 › binding x); or {file, line, start_line?, body}." :type "array" :required? false} notes]}]
  (let [r (pr-report pr)
        {:keys [payload placed]} (review/draft r verdict summary (normalise-notes notes))
        {:keys [repo num]} (:pr r)
        res (json/parse-string (github/gh ["api" "-X" "POST" (str "repos/" repo "/pulls/" num "/reviews") "--input" "-"]
                                          :in (json/generate-string payload))
                               true)]
    (text-result (str "posted " (:state res) " " (:html_url res) "\n" (placement-text placed)))))

(defn ^{:mcp-type :tool
        :mcp-annotations (eg/make-tool-annotations :title "Annotate the review page" :read-only-hint? false :destructive-hint? false :idempotent-hint? true)}
  annotate
  "Attach inferred notes to the review page of a pull request, replacing the
  page's current annotations. Nothing is sent to GitHub. Each annotation:
  {\"on\": {\"file\": path, \"form\": \"defn x\"} | {\"entity\": \":fn.x/y\"} | \"page\",
   \"text\": \"…\", \"basis\": [\"what derived facts it rests on\"], \"author\": \"claude\"}.
  The page shows them labelled inferred, apart from the derived decorations.
  Returns the page URL and every annotation whose target does not resolve."
  [{:keys [^{:doc "Pull request: owner/repo#N or URL" :type "string"} pr
           ^{:doc "JSON array of annotations" :type "array"} annotations]}]
  (let [{:keys [url token]} (ui-server)
        anns (mapv #(merge {:author "claude" :kind "inferred"} (update-keys % keyword)) annotations)
        {:keys [status body]} (http/post (str url "annotate")
                                         {:headers {"content-type" "application/json" "x-sdiff-token" token}
                                          :body (json/generate-string {:pr pr :annotations anns})
                                          :throw false})
        res (json/parse-string body true)]
    (when (not= 200 status) (throw (ex-info (str "review server: " (or (:error res) status)) {})))
    (text-result (str url (subs (:url res) 1)
                      (if (seq (:problems res))
                        (str "\n\n" (count (:problems res)) " annotation(s) do not resolve:\n"
                             (str/join "\n" (for [{:keys [on problem]} (:problems res)] (str "  " (pr-str on) ": " problem))))
                        "\n\nall annotations resolved")))))

(defn ^{:mcp-type :tool
        :mcp-annotations (eg/make-tool-annotations :title "Compose a view" :read-only-hint? false :destructive-hint? false :idempotent-hint? true)}
  view
  "Compose a view of a pull request's review page: the sections a reader should
  see first, in order, under headings with a claim each; everything else stays
  on the page, folded. `view` is EDN:
  {:title \"Safe to merge?\" :question \"…\" :intro \"…\" :author \"claude\"
   :sections [{:title \"…\" :claim \"…\" :folded false
               :items [[:form \"path\" \"defn x\"] [:change \"path\" \"defn x\" \"arity 1 › let 2 › binding y\"]
                       [:entity :fn.x/y] [:file \"path\"] [:header] [:rename \"old\" \"new\"]]}]}
  Items name what the decorated report already shows: a form, one change inside
  it by its printed path, an entity, a file, the registry header, a rename
  roll-up. A folded section is present but closed. Returns the view's URL and
  every item that does not resolve. A PR can hold several named views."
  [{:keys [^{:doc "Pull request: owner/repo#N or URL" :type "string"} pr
           ^{:doc "View name, used in the URL (e.g. safe-to-merge)" :type "string"} name
           ^{:doc "The view, as EDN" :type "string"} view]}]
  (let [{:keys [url token]} (ui-server)
        {:keys [status body]} (http/post (str url "view")
                                         {:headers {"content-type" "application/json" "x-sdiff-token" token}
                                          :body (json/generate-string {:pr pr :name name :view view})
                                          :throw false})
        res (json/parse-string body true)]
    (when (not= 200 status) (throw (ex-info (str "review server: " (or (:error res) status)) {})))
    (text-result (str url (subs (:url res) 1)
                      (if (seq (:problems res))
                        (str "\n\n" (count (:problems res)) " item(s) do not resolve:\n"
                             (str/join "\n" (for [{:keys [section item problem]} (:problems res)] (str "  section " section " " (pr-str item) ": " problem))))
                        "\n\nall items resolved")))))

(defn ^{:mcp-type :prompt :mcp-name "review"} review-prompt
  "Review a pull request for a human reader: derived facts and graph risk first, then what it means, as marked notes and a view."
  [{:keys [^{:doc "Pull request: owner/repo#N or URL" :type "string"} pr]}]
  (eg/make-get-prompt-result
   [(es/make-text-prompt-message
     "user"
     (str "Review " pr " with sdiff, for a human reader.\n\n"
          "1. Call structural-diff for it. Lines marked `derived ·` come from a tool: the structure of each change, the registry's contract diff, and the risk ranking from the system graph. Treat them as facts. Lines marked `inferred ·` are earlier readings; do not restate them.\n"
          "2. Start from the registry header and its risk ranking, then read the forms behind the top risks. Formatting, comments and rename-only files need no reading.\n"
          "3. Decide what the PR means, in the reader's terms: what behaviour changes, for which users or services, what could break and how you would notice, and what the reviewer has to decide. Paths, bindings and line-level detail belong in an annotation's basis, not its text.\n"
          "4. Where a claim needs code the report does not show, read it and say so in the basis.\n"
          "5. Call annotate: one `page` annotation of two or three sentences on what the PR means and how risky it is, then one per form or entity only where something is worth a reviewer's attention (a risk, a decision, a surprise). Write each as a plain statement of consequence; put the evidence in `basis`.\n"
          "6. Compose a view for the question the user is asking (safe to merge? what changes for my service?): sections in the order a reader should take them, each claim one plain sentence about meaning or risk, items the forms and entities that back it. Share the view's URL and the full page's URL.\n"
          "7. Call review-draft with the verdict and only the notes worth sending to the author, and show the result. Call post-review only when the user agrees."))]
   {}))

(def instructions
  "Structural review of Clojure pull requests. Call structural-diff first and
  review from its form paths. Write notes against `form` and `at` exactly as
  printed. Call review-draft, show the user where each note lands and the
  verdict, and call post-review only after they agree.")

(defn -main
  "Run over stdio. MCP traffic is logged to stderr only when SDIFF_MCP_DEBUG is set."
  [& _]
  (ms/run-mcp-server {:traffic-logger (if (System/getenv "SDIFF_MCP_DEBUG") stl/compact-server-traffic-logger stl/nop-traffic-logger)
                      :info (es/make-info "sdiff" "0.1.0" "Structural review of Clojure pull requests")
                      :instructions instructions
                      :vars [#'structural-diff #'review-draft #'post-review #'annotate #'view #'review-prompt]
                      :transport :stdio
                      :print-banner? false}))
