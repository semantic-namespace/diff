(ns sdiff.core-test
  "Fixtures are the changed files of three merged metosin/malli pull requests,
  taken at the PR's merge base (old) and head (new); see test/fixtures/malli.refs.
  Each test states what a reviewer should learn from the file."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [rewrite-clj.node :as n]
            [sdiff.core :as c]
            [sdiff.edn :as e]))

(defn fixture [pr path]
  (let [d (str "test/fixtures/malli-" pr "/")]
    (c/file-report path (slurp (str d "old/" path)) (slurp (str d "new/" path)))))

(defn form [report id] (some #(when (= id (:id %)) %) (:forms report)))
(defn ops [f] (mapv (juxt :op :path) (:changes f)))

(deftest malli-1313-let-becomes-when-let
  (let [r (fixture "1313" "src/malli/error.cljc")
        f (form r ["defn" "^:no-doc -resolve-root-error"])
        [chg] (:changes f)]
    (is (= :semantic (:verdict r)))
    (is (= 1 (count (:forms r))) "one form changed")
    (is (= [[:reshaped [["body"] ["body"] ["body"] ["binding" "[path' m' p']"] ["let 1"]]]] (ops f)))
    (is (= "let" (c/head (:old chg))))
    (is (= "when-let" (c/head (:new chg))))
    (is (some #(= "[schema (mu/get-in schema path)]" (n/string (first %))) (:kept chg))
        "the binding vector survived verbatim")))

(deftest malli-1313-test-adds-one-form
  (let [r (fixture "1313" "test/malli/error_test.cljc")]
    (is (= [[:added-form nil]] (ops (form r ["testing" "\":or #1308\""]))))))

(deftest malli-1315-typed-clj-kondo-output
  (let [r (fixture "1315" "src/malli/clj_kondo.cljc")]
    (testing "a literal body change is one row"
      (let [[chg] (:changes (form r ["defmethod" "accept" ":tuple"]))]
        (is (= [:replaced [["body"]]] ((juxt :op :path) chg)))
        (is (= [":seqable" ":vector"] [(n/string (:old chg)) (n/string (:new chg))]))))
    (testing "a parameter coming into use pairs positionally, and is flagged when the body does not read it"
      (doseq [m [":and" ":andn" ":multi"]]
        (let [f (form r ["defmethod" "accept" m])]
          (is (= [[:replaced [["args"] ["#3"]]]] (ops f)) m)
          (is (= "signature only: parameters changed, body did not" (:note f)) m)))
      (is (nil? (:note (form r ["defmethod" "accept" ":or"]))) "the body reads children"))
    (testing "the `_ → children` pairs are not rolled up as a rename"
      (is (empty? (:renames (c/rollup-renames [r])))))
    (testing "extraction modulo renamed locals"
      (let [f (form r ["defn" "value->type"]) ex (:extraction f)]
        (is (= [[:added-form nil]] (ops f)))
        (is (= "value->type" (:fn ex)))
        (is (= ["defmethod" "accept" ":enum"] (:from ex)))
        (is (= {"child" "lit"} (:renamed ex)))
        (is (= [:dropped] (mapv :op (:drift ex))) "the if-several-types wrapper was not carried over")
        (is (= "value->type" (:extracted (first (:changes (form r ["defmethod" "accept" ":enum"]))))))))
    (testing "a plain new function"
      (is (nil? (:extraction (form r ["defn" "type-set"])))))))

(deftest malli-1315-test-map-keys-pair-by-key
  (let [r (fixture "1315" "test/malli/clj_kondo_test.cljc")
        f (form r ["deftest" "clj-kondo-integration-test"])
        by-key (into {} (for [c (:changes f)] [(last (:path c)) (:op c)]))]
    (is (= {["key" ":any-type-enum"] :removed
            ["key" ":tuple-of-ints"] :replaced
            ["key" ":heterogeneous-type-enum"] :added
            ["key" ":string-or-keyword"] :added}
           by-key))))

(deftest malli-1301-exclusive-bounds
  (let [r (fixture "1301" "src/malli/generator.cljc")]
    (doseq [[m to] [[":<" "gen-fmap"] [":>" "gen-double-above"]]]
      (let [[chg] (:changes (form r ["defmethod" "-schema-generator" m]))]
        (is (= [:reshaped [["body"]]] ((juxt :op :path) chg)) m)
        (is (= ["gen-double" to] [(c/head (:old chg)) (c/head (:new chg))]) m)
        (is (= ["(-child schema options)"] (mapv #(n/string (first %)) (:kept chg))) m)))
    (is (= [[:added-form nil]] (ops (form r ["defn-" "-next-up"]))))
    (is (= [[:added-form nil]] (ops (form r ["defn-" "gen-double-above"]))))))

(deftest rename-rollup
  (let [old "(defn a [x] (:atlas/schema x))\n(defn b [y] (:atlas/schema y))\n"
        new "(defn a [x] (:atlas/data-schema x))\n(defn b [y] (:atlas/data-schema y))\n"
        {:keys [files renames]} (c/rollup-renames [(c/file-report "r.clj" old new)])]
    (is (= [{:from ":atlas/schema" :to ":atlas/data-schema" :count 2}] renames))
    (is (= :rename-only (:verdict (first files))))))

(deftest verdicts-for-non-semantic-changes
  (is (= :whitespace-only (:verdict (c/file-report "w.clj" "(defn a [x]\n  (inc x))" "(defn a [x] (inc x))"))))
  (is (= :comments-only (:verdict (c/file-report "c.clj" "(defn a [x] (inc x))" "(defn a \"adds one\" [x] (inc x))"))))
  (is (= :semantic (:verdict (c/file-report "u.clj" "(defn a [x] (inc x))" "(defn a [x] #_(inc x))"))) "uneval is semantic"))

(deftest canon-handles-a-repeated-local
  (let [[st order] (c/canon (first (c/forms "(+ a a b)")) #{"a" "b"})]
    (is (= [:list "+" [:local 0] [:local 0] [:local 1]] st))
    (is (= ["a" "b"] order))))

(deftest edn-report-is-plain-data-with-positions
  (let [r (fixture "1313" "src/malli/error.cljc")
        f (first (:forms (e/file->edn r)))
        rt (edn/read-string (pr-str f))]
    (is (= (:id f) (:id rt)) "round-trips through the reader")
    (is (string? (get-in f [:new :src])))
    (is (every? pos-int? (map #(get-in f [:new %]) [:row :end-row])) "the head-side form carries its line range")
    (is (= "when-let" (c/head (first (c/forms (get-in f [:changes 0 :new :src]))))))))
