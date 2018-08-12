(ns chia.view.view-spec-test
  (:require [chia.view.view-specs :as s]
            [cljs.test :refer [deftest is are testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]))

(deftest view-specs
  (testing "resolve-spec"

    (s/defspecs {::color {:spec #{:primary :accent}
                          :doc  "Specifies color variable from theme."}
                 ::even? even?})

    ;; def'd function spec
    (is (= (s/resolve-spec ::even?) {:spec even?
                                     :spec-name ::even?}))

    ;; def'd set spec with :doc
    (is (= (s/resolve-spec ::color) {:spec #{:primary :accent}
                                     :doc "Specifies color variable from theme."}))

    ;; inline spec
    (is (= (s/resolve-spec {:spec odd?}) {:spec odd?}))

    ;; primitive
    (is (= (s/resolve-spec :Boolean) {:spec boolean?
                                      :spec-name :Boolean}))

    (is (= (s/spec-kind (s/resolve-spec ::color))
           :Set))
    (is (= (s/spec-kind (s/resolve-spec ::even?))
           ::even?))
    (is (= (s/spec-kind (s/resolve-spec :Function))
           :Function))

    (is (= (s/resolve-spec {:spec :Function})
           {:spec      fn?
            :spec-name :Function}))

    (is (= (s/normalize-props-map {:x ::color
                                   :y {:spec         :Function
                                       :required     true
                                       :pass-through true
                                       :default      "y-value"}})
           {:x {:spec #{:primary :accent}
                :doc "Specifies color variable from theme."}
            :y {:spec fn?
                :spec-name :Function
                :required true
                :pass-through true
                :default "y-value"}
            :props/consumed #{:x}
            :props/defaults {:y "y-value"}}))

    (is (thrown? js/Error (s/validate-spec :x (s/resolve-spec :Function) "s")))
    (is (nil? (s/validate-spec :x (s/resolve-spec :Function) even?)))





    ))