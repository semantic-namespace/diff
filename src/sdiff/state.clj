(ns sdiff.state
  "Per-user review state on disk: settings, and per pull request the forms marked
  viewed, unsent notes, verdict and summary."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-settings
  {:show-inferred true
   :fold-viewed-forms true
   :fold-viewed-files true
   :mark-file-on-github false
   :fold-cosmetic-files false})

(def ^:dynamic *dir* nil)

(defn dir []
  (io/file (or *dir* (System/getenv "SDIFF_STATE_DIR")
               (str (or (System/getenv "XDG_STATE_HOME") (str (System/getProperty "user.home") "/.local/state")) "/sdiff"))))

(defn- read-edn [f default] (if (.exists (io/file f)) (merge default (edn/read-string (slurp f))) default))

(defn- write-edn! [f m]
  (io/make-parents f)
  (let [tmp (io/file (str f ".tmp"))]
    (spit tmp (pr-str m))
    (.renameTo tmp (io/file f))
    m))

(defn- settings-file [] (io/file (dir) "settings.edn"))

(defn settings [] (read-edn (settings-file) default-settings))

(defn save-settings! [m] (write-edn! (settings-file) (merge default-settings (select-keys m (keys default-settings)))))

(defn- review-file [repo num]
  (let [[owner name] (str/split repo #"/")]
    (io/file (dir) "reviews" owner name (str num ".edn"))))

(def empty-review {:forms {} :notes [] :verdict "comment" :summary "" :annotations []})

(defn review [repo num] (read-edn (review-file repo num) empty-review))

(defn save-review!
  "Merges `m` into the stored review, so the panel's save keeps annotations and
  an annotate call keeps notes."
  [repo num m]
  (write-edn! (review-file repo num) (merge (review repo num) (select-keys m (keys empty-review)))))
