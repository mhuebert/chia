(ns chia.graphql.string-test
  (:require [chia.graphql.string :as string]
            [cljs.test :as t :refer [is are]]
            [clojure.string :as str]))

(t/deftest emit

  (is (= (string/emit-value {:a 1 :b 2}) "{a: 1, b: 2}")
      "Maps are emitted with {}")

  (is (= (string/emit-value ^::string/props {:a 1 :b 2}) "(a: 1, b: 2)")
      "Props are emitted with ()")

  (are [data out]
    (= (string/emit-value data) out)

    "hello" "\"hello\""
    4 4
    [1 2] "[1, 2]"
    true "true"
    false "false"
    nil "null"
    :id "id"
    :id! "id!"
    :id+ "[id]"
    :id+! "[id]!"
    :id!+ "[id!]"
    :id!+! "[id!]!")

  (are [form out]
    (is (= (:string (string/emit form)) out))
    [:... :id :name] "id\nname"
    [:... {:on :Pet} :id :name] "... on Pet {\n  id\n  name\n  __typename\n}"
    [:x {:graphql/alias-of :y
         :z 1} :id] "x: y(z: 1) {\n  id\n  __typename\n}")

  (is (= (:string (string/emit [:entity [:... :id]]))
         (:string (string/emit [:entity :id])))
      "Splice fragments"))

