(ns sdiff.render.html
  "A self-contained HTML page for one or more pull requests. Files are sorted by
  verdict and only behaviour-changing files are expanded; each form is shown
  whole, with the changed and kept parts marked in the source."
  (:require [rewrite-clj.node :as n]
            [clojure.string :as str]
            [hiccup2.core :as hc]
            [sdiff.core :refer [fmt-path head short index]]))

(defn- offsets [src]
  (let [starts (reductions + 0 (map #(inc (count %)) (str/split src #"\n" -1)))]
    (fn [row col] (+ (nth starts (dec row)) (dec col)))))

(defn- range-of [off node]
  (let [{:keys [row col end-row end-col]} (meta node)]
    (when row [(off row col) (off end-row end-col)])))

(defn- render-ranges
  "Wrap `[s e cls]` ranges (sorted, nesting allowed, no partial overlap) in marks over src[from,to)."
  [src from to rs]
  (loop [pos from rs rs out []]
    (if-let [[s e cls] (first rs)]
      (let [inner (take-while (fn [[s2 e2]] (and (>= s2 s) (<= e2 e))) (rest rs))
            after (drop (count inner) (rest rs))]
        (recur e after (conj out (subs src pos s) (into [:mark {:class cls}] (render-ranges src s e inner)))))
      (conj out (subs src pos to)))))

(defn- highlighted
  "Source of `form` with `marks` (`[node class]`) wrapped in spans; marks may nest.
  Synthetic nodes without position metadata are skipped."
  [src form marks]
  (let [off (offsets src)
        [fs fe] (range-of off form)
        rs (sort-by (fn [[s e]] [s (- e)])
                    (for [[node cls] marks :let [r (range-of off node)] :when r
                          :let [[s e] r] :when (and (>= s fs) (<= e fe))] [s e cls]))]
    (render-ranges src fs fe rs)))

(defn- marks-for [side changes]
  (concat
   (for [{:keys [op old new rename extracted]} changes
         :let [node (if (= side :old) old new)]
         :when (and node (#{:replaced :added :removed :reshaped} op))]
     [node (cond rename "ren" extracted "ext" (= side :old) "del" :else "add")])
   (for [{:keys [kept]} changes [o nw] kept]
     [(if (= side :old) o nw) "kept"])))

(defn- kept-codes [kept] (interpose ", " (for [[o] kept] [:code.kept (short (n/string o))])))

(defn- move-note [{:keys [moved-to moved-from kept]}]
  (cond moved-to   [:span.what "moved to " [:code (fmt-path moved-to)] ": " (kept-codes kept)]
        moved-from [:span.what "takes over from " [:code (fmt-path moved-from)] ": " (kept-codes kept)]))

(defn- change-row [{:keys [op path old new extracted rename kept] :as c}]
  (let [p (fmt-path path) mv (move-note c)]
    (case op
      :comments [:li.chg.chg-note [:span.p (or (not-empty p) "form")] [:span.what "comments or docstring only"]]
      :wrapper  [:li.chg.chg-sem [:span.p "wrapper"] [:span.what (str (name (n/tag old)) " removed → " (name (n/tag new)))]]
      :added    [:li.chg.chg-sem [:span.p p] [:code.add (short (n/string new))] mv]
      :removed  [:li.chg.chg-sem [:span.p p] [:code.del (short (n/string old))] mv]
      :dropped  [:li.chg.chg-sem [:span.p "around the extracted part, not carried into the new function"]
                 [:code.del (short (n/string old))]]
      :reshaped [:li.chg.chg-sem [:span.p p [:em.reshape (str " restructured " (or (head old) (name (n/tag old))) " → " (or (head new) (name (n/tag new))))]]
                 [:code.del (short (n/string old))] [:code.add (short (n/string new))]
                 [:span.what "kept as-is: " (kept-codes kept)]]
      :replaced (if rename
                  [:li.chg.chg-ren [:span.p p] [:span.what "part of rename " [:code (first rename)] " → " [:code (second rename)]]]
                  [:li.chg.chg-sem [:span.p p (when extracted [:em.ext (str " extracted → " extracted)])]
                   [:code.del (short (n/string old))] [:code.add (short (n/string new))] mv])
      nil)))

(defn- form-view [{:keys [old new]} {:keys [id changes extraction note]}]
  (let [op0 (:op (first changes))
        full? (#{:added-form :removed-form} op0)
        node-old (when-not (= op0 :added-form) (get (index old) id))
        node-new (when-not (= op0 :removed-form) (get (index new) id))]
    [:section.form
     [:h3 [:code (str/join " " (map str id))]
      (case op0 :added-form [:span.tag.tag-add "new"] :removed-form [:span.tag.tag-del "removed"] nil)
      (when extraction [:span.tag.tag-ext (str "extracted from " (str/join " " (map str (:from extraction))))])
      (when note [:span.tag.tag-note note])]
     (when-not full?
       [:ul.changes (map change-row changes)])
     (when extraction
       [:div.drift
        (when (seq (:renamed extraction))
          [:p "Locals renamed: " (interpose ", " (for [[o nw] (:renamed extraction)] [:span [:code o] " → " [:code nw]]))])
        [:p (if (seq (:drift extraction)) "Compared with the expression it replaced, the new body differs in:" "The body is the replaced expression, unchanged.")]
        [:ul.changes (map change-row (:drift extraction))]])
     [:details {:open (boolean full?)}
      [:summary (if full? "source" "whole form, changes marked")]
      [:div.sbs {:class (when full? "single")}
       (when node-old [:pre.code (seq (highlighted old node-old (marks-for :old changes)))])
       (when node-new [:pre.code (seq (highlighted new node-new (marks-for :new changes)))])]]]))

(def verdict-label {:semantic "changes behaviour" :rename-only "rename only" :comments-only "comments only" :whitespace-only "formatting only"})
(def verdict-order {:semantic 0 :rename-only 1 :comments-only 2 :whitespace-only 3})

(defn- file-view [gh-url pr-num {:keys [path verdict forms status] :as fr}]
  [:article.file {:class (name verdict) :id (str "f-" (hash path))}
   [:h2 [:span.verdict (verdict-label verdict)]
    [:code.path path]
    (case status "A" [:span.tag.tag-add "new file"] "D" [:span.tag.tag-del "deleted"] nil)
    (when gh-url [:a.gh {:href (str gh-url "/pull/" pr-num "/files") :target "_blank"} "comment on GitHub"])]
   (when (= verdict :semantic) (map (partial form-view fr) forms))
   (when (#{:comments-only :rename-only} verdict)
     [:details [:summary "forms touched"]
      [:ul (for [f forms] [:li [:code (str/join " " (map str (:id f)))]
                           (for [c (:changes f) :when (:rename c)] [:span.where (str " — " (fmt-path (:path c)))])])]])])

(defn- pr-view [gh-url {:keys [num title clj other renames]}]
  (let [counts (frequencies (map :verdict clj))]
    [:section.pr {:id (str "pr-" num)}
     [:header
      [:h1 (if gh-url [:a {:href (str gh-url "/pull/" num) :target "_blank"} (str "#" num)] (str "#" num)) " " title]
      [:p.sum
       (str (count clj) " Clojure file" (when (not= 1 (count clj)) "s") ": ")
       (str/join ", " (for [[k l] [[:semantic "change behaviour"] [:rename-only "rename only"] [:comments-only "comments only"] [:whitespace-only "formatting only"]] :when (counts k)] (str (counts k) " " l)))
       (when (seq other) (str "; " (count other) " other file" (when (not= 1 (count other)) "s")))]
      (when (seq renames)
        [:ul.renames (for [{:keys [from to count]} renames]
                       [:li "rename " [:code from] " → " [:code to] [:span.n (str count " sites across the PR")]])])]
     (map (partial file-view gh-url num) (sort-by (comp verdict-order :verdict) clj))
     (when (seq other)
       [:details.other [:summary (str (count other) " non-Clojure files" (when gh-url ", shown by GitHub"))]
        [:ul (for [{:keys [path]} other] [:li (if gh-url [:a {:href (str gh-url "/pull/" num "/files") :target "_blank"} path] path)])]])]))

(def css (slurp (clojure.java.io/resource "sdiff/report.css")))

(defn page
  "Whole page for `prs`, each a report with `:num` and `:title`. `gh-url` is the
  repository URL used for links, or nil for a page without GitHub links."
  [gh-url repo-name prs]
  (str "<!doctype html>"
       (hc/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1, viewport-fit=cover"}]
          [:title (str "Structural review — " repo-name)]
          [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
          [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap"}]
          [:style (hc/raw css)]]
         [:body
          [:div.wrap
           [:div.intro
            [:h1 "What changed, by form"]
            [:p (str (count prs) " pull request" (when (not= 1 (count prs)) "s") " from ") [:code repo-name]
             ", read structurally. Each file is sorted into one of four verdicts; only files that change behaviour are expanded. Inside those, every change is named by its place in the code — the binding, clause or step it lives in — rather than by line."]]
           [:nav.toc (for [{:keys [num title]} prs] [:a {:href (str "#pr-" num)} (str "#" num " " title)])]
           (map (partial pr-view gh-url) prs)]]])))
