(ns sdiff.state-test
  (:require [clojure.test :refer [deftest is]]
            [sdiff.state :as state]))

(defn- tmp-dir [] (str (java.nio.file.Files/createTempDirectory "sdiff-state" (make-array java.nio.file.attribute.FileAttribute 0))))

(deftest settings-default-and-persist
  (binding [state/*dir* (tmp-dir)]
    (is (= state/default-settings (state/settings)))
    (state/save-settings! {:fold-viewed-forms false :unknown 1})
    (is (= (assoc state/default-settings :fold-viewed-forms false) (state/settings)))))

(deftest review-state-per-pull-request
  (binding [state/*dir* (tmp-dir)]
    (is (= state/empty-review (state/review "owner/repo" 7)))
    (state/save-review! "owner/repo" 7 {:forms {"src/a/b.clj|defn f" "123"} :notes [{:file "src/a/b.clj" :form "defn f" :body "x"}]})
    (is (= {"src/a/b.clj|defn f" "123"} (:forms (state/review "owner/repo" 7))))
    (is (= 1 (count (:notes (state/review "owner/repo" 7)))))
    (is (= state/empty-review (state/review "owner/repo" 8)) "another PR starts empty")))
