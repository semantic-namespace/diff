(ns sdiff.deps-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [sdiff.deps :as deps]))

(def files
  {"src/app/port.clj" "(ns app.port)\n(defprotocol Store (save! [s x]))\n"
   "src/app/pg.clj" "(ns app.pg (:require [app.port :as port] [next.jdbc :as jdbc]))\n(defrecord Pg [ds]\n  port/Store\n  (save! [_ x] (jdbc/execute! ds [x])))\n"
   "src/app/q.clj" "(ns app.q (:require [hugsql.core :as hugsql]))\n(hugsql/def-db-fns \"app/q.sql\")\n(defn lookup [db id] (db-find db {:id id}))\n"
   "src/app/core.clj" "(ns app.core (:require [app.port :as port] [app.q :as q] [app.cycle-a]))\n(defn persist [s x] (port/save! s x))\n(defn find [db id] (q/lookup db id))\n(defn run [s db] (persist s 1) (find db 1))\n"
   "src/app/cycle_a.clj" "(ns app.cycle-a (:require [app.cycle-b]))\n"
   "src/app/cycle_b.clj" "(ns app.cycle-b (:require [app.cycle-a]))\n"
   "src/app/spec.clj" "(ns app.spec (:require [clojure.spec.alpha :as s]))\n(s/def ::purpose keyword?)\n"
   "src/app/use.clj" "(ns app.use (:require [app.spec :as spec]))\n(defn tag [x] {::spec/purpose x})\n"
   "test/app/mock.clj" "(ns app.mock (:require [app.port :as port] [app.core :as core]))\n(defrecord Fake [] port/Store (save! [_ x] x))\n(defn t [] (core/persist (->Fake) 1))\n"})

(defn- project []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "deps" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[p s] files] (let [f (io/file root p)] (io/make-parents f) (spit f s)))
    (str root)))

(defn- node [g var-name] (some (fn [[k v]] (when (= var-name (:var v)) k)) (:forms g)))

(deftest the-whole-repository-graph
  (let [g (deps/graph (project) deps/defaults)
        callers (fn [v] (set (keep #(get-in g [:forms % :var]) (for [[from tos] (:calls g) :when (tos (node g v))] from))))]
    (is (= #{"app.core/run" "app.mock/t"} (callers "app.core/persist")) "callers across namespaces, tests included")
    (is (contains? (get-in g [:ext (node g "app.q/lookup")]) "hugsql (generated)") "a HugSQL query is a call to a library")
    (is (contains? (get-in g [:io (node g "app.q/lookup")]) :postgres))
    (is (some #(= :postgres %) (mapcat val (select-keys (:io g) (get-in g [:calls (node g "app.core/persist")]))))
        "a protocol with one implementation outside tests is followed")
    (is (= #{#{"app.cycle-a" "app.cycle-b"}} (#'deps/cycles (:ns-deps g) (:project g))))))

(defn- spec-edge? [g]
  (let [sdef (some (fn [[k v]] (when (re-find #"s/def ::purpose" (:name v)) k)) (:forms g))
        tag (some (fn [[k v]] (when (= "app.use/tag" (:var v)) k)) (:forms g))]
    (contains? (get-in g [:calls tag]) sdef)))

(deftest a-keyword-is-no-dependency-unless-a-host-says-so
  (is (not (spec-edge? (deps/graph (project) deps/defaults))) "sdiff reads no meaning into a keyword"))

(deftest a-host-installs-a-keyword-bridge
  (deps/add-bridge! ::spec (deps/keyword-bridge #"^clojure\.spec\.alpha/def$"))
  (try (is (spec-edge? (deps/graph (project) deps/defaults)) "a spec keyword depends on its s/def")
       (finally (swap! @#'deps/bridges (fn [bs] (vec (remove #(= ::spec (first %)) bs)))))))

(deftest a-new-form-only-says-who-calls-it
  (let [parts (#'deps/line-parts {:new? true :callers ["a/b"] :caller-ns 1 :test-callers 0 :callers-before 0 :outside-callers 1
                                  :calls-added ["x/y"] :libs-added ["lib"] :reaches-added [:postgres]})]
    (is (= 2 (count parts)) "callers, and the I/O it brings")
    (is (re-find #"1 caller" (first parts)))))

(deftest callers-from-the-same-namespace-alone-are-not-worth-a-line
  (is (empty? (#'deps/line-parts {:callers ["a/b"] :caller-ns 1 :test-callers 0 :callers-before 1 :outside-callers 0}))))

(deftest a-test-namespace-requiring-code-is-not-new-coupling
  (let [d {:head {:forms {["test/a_test.clj" 1] {:ns "a-test" :test true}}}}]
    (is (= [["a" "b"]] (#'deps/couplings d [{:ns "a" :internal-added ["b"]} {:ns "a-test" :internal-added ["a"]}])))))

(deftest the-snapshot-carries-what-short-names-need
  (let [g (deps/graph (project) deps/defaults)
        report {:clj [{:path "src/app/core.clj"} {:path "src/app/use.clj"}]}
        a (deps/names-analysis {:base g :head g} report)]
    (is (some #(and (= 'app.q (:to %)) (= 'q (:alias %))) (:namespace-usages a)))
    (is (some #(= 'app.core (:name %)) (:namespace-definitions a)))
    (is (not-any? #(= "src/app/pg.clj" (:filename %)) (:namespace-usages a)) "only the PR's files")))

(deftest the-cache-keeps-only-what-is-in-use
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "cache" (make-array java.nio.file.attribute.FileAttribute 0)))
        day (* 24 3600 1000) now (System/currentTimeMillis)
        put (fn [p age] (let [f (io/file root p)] (io/make-parents f) (spit f "") (.setLastModified f (- now (* age day))) f))
        sha #(apply str (repeat 40 %))]
    (put (str "src/o--r/" (sha "a") "/.complete") 30)
    (put (str "src/o--r/" (sha "b") "/.complete") 1)
    (.setLastModified (.getParentFile (put (str "src/o--r/" (sha "c") "/x.clj") 2)) (- now (* 2 day)))
    (put (str "deps/o--r-" (sha "b") "-1.edn") 3)
    (put (str "deps/o--r-" (sha "b") "-2.edn") 1)
    (put (str "deps/o--r-" (sha "a") "-1.edn") 30)
    (deps/sweep! root 14)
    (is (not (.exists (io/file root "src/o--r" (sha "a")))) "unused for a month")
    (is (.exists (io/file root "src/o--r" (sha "b"))) "used yesterday")
    (is (not (.exists (io/file root "src/o--r" (sha "c")))) "a download that never completed")
    (is (= #{(str "o--r-" (sha "b") "-2.edn")} (set (map #(.getName %) (.listFiles (io/file root "deps"))))) "the newest snapshot of a commit in use")))

(deftest a-bridge-is-told-what-it-reads
  (let [seen (atom nil)
        root (project)]
    (deps/add-bridge! ::probe (fn [ctx] (reset! seen (dissoc ctx :analysis :form-at)) []))
    (try (deps/graph root (assoc deps/defaults :repo "o/r" :sha "abc"))
         (finally (swap! @#'deps/bridges (fn [bs] (vec (remove #(= ::probe (first %)) bs))))))
    (is (= {:root (.getCanonicalPath (io/file root)) :repo "o/r" :sha "abc"} @seen))))

(deftest a-repository-states-its-own-instructions
  (is (nil? (deps/parse-repo-file "")) "no file, or an empty one, says nothing")
  (is (= {:io {:datomic "^datomic\\."}} (deps/parse-repo-file "{:io {:datomic \"^datomic\\\\.\"}}")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not :iox" (deps/parse-repo-file "{:iox {}}"))
      "a key sdiff does not read is an error, not ignored"))

(deftest the-heads-instructions-read-both-sides
  (let [asked (atom [])]
    (with-redefs [sdiff.github/blob (fn [repo sha path] (swap! asked conj [repo sha path])
                                      (if (= sha "head") "{:io {:datomic \"^datomic\\\\.\"} :test-paths \"^spec/\"}" ""))]
      (reset! @#'deps/repo-files {})
      (let [c (#'deps/config-for "o/r" "head")]
        (is (= "^datomic\\." (get-in c [:io :datomic])) "added to the table")
        (is (= (get-in deps/defaults [:io :postgres]) (get-in c [:io :postgres])) "the rest of the table kept")
        (is (= "^spec/" (:test-paths c)))
        (is (= [["o/r" "head" "diff.edn"]] @asked))))))

(deftest a-repository-file-reaches-the-graph
  (let [g (deps/graph (project) (merge-with merge deps/defaults (deps/parse-repo-file "{:io {:sql \"^next\\\\.jdbc\"}}")))
        pg (some (fn [[k v]] (when (re-find #"defrecord Pg" (:name v)) k)) (:forms g))]
    (is (= #{:postgres :sql} (get-in g [:io pg])) "the repository's kind beside the built-in one")))
