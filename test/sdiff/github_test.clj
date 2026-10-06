(ns sdiff.github-test
  (:require [clojure.test :refer [deftest is]]
            [sdiff.github :as github]
            [sdiff.render.html :as html]))

(def reviews
  [{:user {:login "me"} :state "COMMENTED" :submitted_at "2026-10-01T09:00:00Z"}
   {:user {:login "me"} :state "APPROVED" :submitted_at "2026-10-02T09:00:00Z" :html_url "u1"}
   {:user {:login "ana"} :state "CHANGES_REQUESTED" :submitted_at "2026-10-02T10:00:00Z"}
   {:user {:login "ana"} :state "APPROVED" :submitted_at "2026-10-03T10:00:00Z"}
   {:user {:login "bo"} :state "COMMENTED" :submitted_at "2026-10-03T11:00:00Z"}])

(deftest the-latest-standing-review-per-user-counts
  (let [{:keys [mine others]} (github/standing reviews "me")]
    (is (= {:login "me" :state "APPROVED" :at "2026-10-02" :url "u1"} mine))
    (is (= [{:login "ana" :state "APPROVED" :at "2026-10-03" :url nil}] others) "a comment-only reviewer has no standing")))

(deftest the-status-line
  (is (= "open · you approved 2026-10-02 · ana approved 2026-10-03"
         (html/status-text (merge {:state "open" :author "x" :viewer "me"} (github/standing reviews "me")))))
  (is (= "merged 2026-10-04 · your own PR"
         (html/status-text {:state "merged" :merged-at "2026-10-04" :author "me" :viewer "me"}))))
