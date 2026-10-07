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
