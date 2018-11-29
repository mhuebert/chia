(ns chia.graphql.schema-test
  (:require [chia.graphql.schema :as schema]
            [chia.graphql.schema.resolve :as r]
            ["graphql" :as graphql]
            [cljs.test :as t :refer [deftest is]]
            [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as st]))




(deftest instrument-varargs

  (defn defx [key & [doc]]
    key)

  (s/fdef defx
          :args (s/cat :key keyword?
                       :doc (s/? string?)))

  (st/instrument)

  (is (defx :a ""))

  (is (thrown? js/Error (defx 1 1)))

  (is (thrown? js/Error (defx 1 1 1 1)))

  (is (thrown? js/Error (defx :a "" 1))))


(t/deftest utility-functions

  (is (= (schema/object :Person {})
         :Person))

  (is (= (schema/object ::Pet
           "A description"
           {:name :String})
         ::Pet))

  (is (= (schema/external-keys ::Thread [::Entity.x
                                         ::Thread.y
                                         :z])
         #{::Entity}))


  (is (= (schema/typespace ::A.b) ::A))
  (is (= (schema/typespace ::A) ::A))
  (is (= (schema/typekey ::A.b) :b))
  (is (= (schema/keyword-append ::A.b "!")
         ::A.b!))
  (is (= (schema/typekey-append ::A.b "c")
         ::A.b.c)))

(do #_#_t/deftest make-schema
  (reset! schema/*registry-ref* {})

  (schema/interface ::Named
    {:name :String})

  (schema/enum ::FoodCategory
    [:VEGETABLES
     :FRUIT
     :MEAT])

  (schema/object ::Person
    {::Named.name :String
     :age :Int
     :eats [::FoodCategory]})

  (schema/object ::Pet
    {::Named.name :String!
     :age :Int
     :eats [::FoodCategory]})

  (schema/union ::LifeForm
    [::Person
     ::Pet])

  (schema/object ::Household
    {:members [::LifeForm]})

  (schema/defquery household [{:id :String!}]
                   ::Household)

  (is (= (-> (schema/get-key ::Pet.name)
             :implementation-of
             (= ::Named.name)))
      "Keys that implement an interface record the interface in registry")

  (doseq [[user-key expected-type-key] [[::LifeForm :Union]
                                        [::Pet :Object]
                                        [::Named :Interface]
                                        [::FoodCategory :Enum]]]
    (is (= (-> (schema/get-key user-key)
               :type-key
               (= expected-type-key)))))



  (let [Schema (r/make-schema {:query {:household household}})
        prev-str "enum FoodCategory {\n  VEGETABLES\n  FRUIT\n  MEAT\n}\n\ntype Household {\n  members: [LifeForm]\n}\n\nunion LifeForm = Person | Pet\n\ninterface Named {\n  name: String\n}\n\ntype Person implements Named {\n  name: String\n  age: Int\n  eats: [FoodCategory]\n}\n\ntype Pet implements Named {\n  name: String!\n  age: Int\n  eats: [FoodCategory]\n}\n\ntype Query {\n  household(id: String!): Household\n}"
        prev-Schema (graphql/buildSchema prev-str)]

    (is (empty? (graphql/findBreakingChanges prev-Schema
                                             Schema)))
    (is (empty? (graphql/findDangerousChanges prev-Schema
                                              Schema)))))