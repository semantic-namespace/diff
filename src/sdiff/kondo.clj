(ns sdiff.kondo
  "Static analysis of the PR's sources with clj-kondo: namespaces, the aliases
  each file gives them, resolved keywords and call edges. No evaluation, no
  classpath: each side's text is written under its real path and analysed
  once. Runs clj-kondo as a library on the JVM and as a pod on babashka;
  returns nil when neither is available, so callers can fall back."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private pod-version "2026.01.19")

(defn- runner []
  (if (System/getProperty "babashka.version")
    (try ((requiring-resolve 'babashka.pods/load-pod) 'clj-kondo/clj-kondo pod-version)
         (requiring-resolve 'pod.borkdude.clj-kondo/run!)
         (catch Exception _ nil))
    (try (requiring-resolve 'clj-kondo.core/run!) (catch Exception _ nil))))

(defonce ^:private run (delay (runner)))

(defn- tmp-dir [] (.toFile (java.nio.file.Files/createTempDirectory "sdiff-kondo" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- relative [^java.io.File root filename]
  (let [r (str (.getCanonicalPath root) "/") f (str filename)]
    (if (str/starts-with? f r) (subs f (count r)) f)))

(defn analyse
  "clj-kondo analysis of `files` (`{path source}`), with every `:filename`
  given back as the PR path, or nil when clj-kondo is unavailable."
  [files]
  (when-let [run! @run]
    (let [root (tmp-dir)]
      (try
        (doseq [[path src] files :when (seq src)]
          (let [f (io/file root path)] (io/make-parents f) (spit f src)))
        (let [a (:analysis (run! {:lint [(str root)]
                                  :config {:analysis {:keywords true :var-usages true :var-definitions true}
                                           :output {:canonical-paths true}}}))]
          (into {} (for [[k xs] a]
                     [k (mapv #(cond-> % (:filename %) (update :filename (partial relative root))) xs)])))
        (finally
          (doseq [f (reverse (file-seq root))] (.delete ^java.io.File f)))))))

(defn lint
  "clj-kondo's full result for `paths` under `config`, or nil when clj-kondo is unavailable."
  [paths config]
  (when-let [run! @run] (run! {:lint (mapv str paths) :config config})))
