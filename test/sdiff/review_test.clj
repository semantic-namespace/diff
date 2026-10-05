(ns sdiff.review-test
  "Placement of review notes on the atlas #7 fixture. Hunks come from
  `git diff --no-index` of the fixture's two sides, the same -U3 context GitHub
  uses, so a note lands where GitHub would accept it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [sdiff.core :as c]
            [sdiff.hunks :as hunks]
            [sdiff.review :as review]))

(def path "core/src/atlas/registry.cljc")
(def old-src (slurp (str "test/fixtures/atlas-7/old/" path)))
(def new-src (slurp (str "test/fixtures/atlas-7/new/" path)))

(def report
  (let [patch (:out (sh "git" "diff" "--no-index" "-U3"
                        (str "test/fixtures/atlas-7/old/" path) (str "test/fixtures/atlas-7/new/" path)))]
    {:head "f442fa107a9289ce6ed904d309130cad93c495e8"
     :clj [(assoc (c/file-report path old-src new-src) :hunks (hunks/right-ranges patch))]}))

(defn line-of [s] (inc (count (filter #{\newline} (subs new-src 0 (str/index-of new-src s))))))

(deftest hunk-ranges
  (is (= [[3 7] [20 29]] (hunks/right-ranges "@@ -3,4 +3,5 @@ x\n@@ -20 +20,10 @@\n@@ -40,2 +39,0 @@\n")))
  (is (= [5 7] (hunks/clamp [[1 2] [5 9]] [4 7])))
  (is (nil? (hunks/clamp [[1 2]] [4 7]))))

(deftest a-change-path-lands-on-its-new-expression
  (let [{:keys [inline]} (review/place report {:file path :form "defn compile!"
                                              :at "arity 1 › let 2 › binding dev-id-conflicts"
                                              :body "why extract?"})]
    (is (= {:path path :line (line-of "(dev-id-conflicts entries)") :side "RIGHT" :body "why extract?"} inline))))

(deftest a-bare-name-resolves-when-unique
  (let [{:keys [inline]} (review/place report {:file path :form "dev-id-conflicts" :body "nice"})]
    (is (= (line-of "(defn dev-id-conflicts") (:line inline)))
    (is (nil? (:start_line inline)) "a form-level note is one line")))

(deftest notes-github-would-refuse-go-to-the-body
  (testing "a line outside every hunk"
    (let [p (review/place report {:file path :line 1 :body "ns"})]
      (is (nil? (:inline p)))
      (is (str/includes? (:body p) "outside the PR's diff"))))
  (testing "a form that did not change"
    (is (str/includes? (:body (review/place report {:file path :form "defn no-such-thing" :body "?"})) "no changed form")))
  (testing "a file the PR does not touch"
    (is (str/includes? (:body (review/place report {:file "x.clj" :form "defn a" :body "?"})) "no changed Clojure file"))))

(deftest the-payload
  (let [{:keys [payload placed]}
        (review/draft report "request-changes" "Two questions."
                      [{:file path :form "defn compile!" :at "arity 1 › let 2 › binding dev-id-conflicts" :body "inline"}
                       {:file path :line 1 :body "about the ns"}])]
    (is (= "REQUEST_CHANGES" (:event payload)))
    (is (= (:head report) (:commit_id payload)))
    (is (= ["inline"] (mapv :body (:comments payload))))
    (is (str/includes? (:body payload) "about the ns") "the refused note is kept in the body")
    (is (= 2 (count placed))))
  (is (= "APPROVE" (:event (:payload (review/draft report :approve nil [])))) "an empty approval is allowed")
  (is (thrown? Exception (review/draft report "lgtm" nil [])))
  (is (thrown? Exception (review/draft report "comment" "" [])) "a comment review needs content"))

(deftest notes-may-use-what-the-text-report-prints
  (let [line (line-of "(dev-id-conflicts entries)")
        lands (fn [note] (-> (review/draft (assoc report :names {:paths {path "registry.cljc"} :nss {}}) "comment" "" [(assoc note :body "b")])
                             :payload :comments first :line))]
    (testing "a short file name from the legend"
      (is (= line (lands {:file "registry.cljc" :form "defn compile!" :at "arity 1 › let 2 › binding dev-id-conflicts"}))))
    (testing "the end of a path, as printed after an elided start"
      (is (= line (lands {:file path :form "defn compile!" :at "‥ › binding dev-id-conflicts"})))
      (is (= line (lands {:file path :form "defn compile!" :at "binding dev-id-conflicts"}))))))
