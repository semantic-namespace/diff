(ns sdiff.names-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [sdiff.names :as names]))

(def a-src "(ns co.acme.billing.invoice (:require [co.acme.billing.spec.invoice :as spec.invoice] [co.acme.shared.money :as money]))\n(defn total [x] (::spec.invoice/lines x) (money/sum x))")
(def b-src "(ns co.acme.billing.payment (:require [co.acme.billing.spec.invoice :as spec.invoice] [co.acme.shared.money :as m]))\n(defn pay [x] (m/sum x))")
(def t-src "(ns co.acme.billing.invoice-test (:require [co.acme.billing.invoice :as SUT]))")

(def report {:clj [{:path "src/co/acme/billing/invoice.clj" :new a-src :old a-src}
                   {:path "src/co/acme/billing/payment.clj" :new b-src :old b-src}
                   {:path "test/co/acme/billing/invoice_test.clj" :new t-src :old t-src}]
             :other [{:path "resources/billing/invoice.edn"}]})

(deftest names-come-from-the-authors-aliases-then-unique-suffixes
  (let [{:keys [paths nss]} (names/table report)]
    (is (= "spec.invoice" (nss "co.acme.billing.spec.invoice")) "the alias the source files agree on")
    (is (= "money" (nss "co.acme.shared.money")) "ties go to one alias, consistently")
    (is (not= "SUT" (nss "co.acme.billing.invoice")) "a test file's alias does not name a source namespace")
    (is (= "invoice.clj" (paths "src/co/acme/billing/invoice.clj")))
    (is (= "invoice.edn" (paths "resources/billing/invoice.edn")))))

(deftest rewriting-leaves-source-and-attributes-exact
  (let [t (names/table report)
        h [:div {:data-file "src/co/acme/billing/invoice.clj"}
           [:p "in src/co/acme/billing/invoice.clj: :co.acme.billing.spec.invoice/lines"]
           [:pre.code "(ns co.acme.billing.spec.invoice)"]]
        out (names/shorten-hiccup t h)]
    (is (= {:data-file "src/co/acme/billing/invoice.clj"} (second out)))
    (is (= "in invoice.clj: :spec.invoice/lines" (second (nth out 2))))
    (is (= [:pre.code "(ns co.acme.billing.spec.invoice)"] (nth out 3)))
    (is (= {"co.acme.billing.spec.invoice" "spec.invoice"}
           (:nss (names/used t "only :co.acme.billing.spec.invoice/lines here"))) "the legend lists only names the page uses")))
