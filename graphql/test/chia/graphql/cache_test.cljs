(ns chia.graphql.cache-test
  (:require [cljs.test :as test :refer [deftest is are testing]]
            [chia.graphql.normalize :as n]
            [chia.graphql.schema :as schema]
            [chia.triple-db.core :as d]
            [chia.graphql.root :as root]))

(def parent-fields
  [:.../parent-fields {:on :Human}
   :id :name])

(def pet-fields
  [:.../pet-fields {:on :Pet}
   :id :name])

(def human-fields
  [:.../human-fields {:on :Entity}
   :id
   [:father
    parent-fields]
   :name
   [:NAME {:graphql/alias-of :name}]

   [:photo {:width 10
            :height 10}]
   [:pets pet-fields]])

(def human
  [:.../human {:on :Entity}
   human-fields])

(defn req-keys [x]
  (if (vector? x)
    (list x)
    (:root/xkeys x)))

#_(s/fdef req-keys
          :args (s/cat :req (s/or :req :graphql/root
                                  :k :graphql/xkey)))

(def cache
  (d/create))

(defn round-trip [request data]
  (let [cache (-> (d/create)
                  (root/add-to-cache! request
                                      (clj->js {:data data})))]
    (root/read-root cache request)))

(test/deftest graphql-cache

  (is (-> (round-trip
           (schema/query me
             {:root/xkeys [:name]} [])
           {:me {:name "Henry"}})
          :me
          :name
          (= "Henry"))
      "A single key is returned directly")

  (is (-> (round-trip
           (schema/mutation me
             {:root/xkeys [:name]} [])
           {:me {:name "Henry"}})
          :me
          :name
          (= "Henry"))
      "Mutation works like query")

  (is (let [cache (d/create)
            _ (root/add-to-cache! cache
                                  [:... :id :name
                                   [:pets :id :name]]
                                  (clj->js {:data {:id "A"
                                                   :name "Henry"
                                                   :pets [{:id "B"
                                                           :name "Bertrand"}]}}))
            result (n/read-keys cache "A"
                                :id
                                :name
                                [:pets
                                 :id :name])]
        (and (= (:name result) "Henry")
             (= (get-in result [:pets 0 :name]) "Bertrand")))
      "Data is normalized by id")

  (let [named-fragment [:.../name {:on "Person"} :name]
        inline-fragment [:... {:on "Person"} :hobby]
        anonymous-fragment [:... :happy?]]
    (is (-> (round-trip (-> (schema/query me
                              {:root/xkeys [named-fragment
                                               inline-fragment
                                               anonymous-fragment
                                               [:... {:on "Pet"} :furry?]]}
                              []))
                        {:me {:name "Henry"
                              :hobby "curling"
                              :furry? true
                              :happy? true
                              :__typename "Person"}})
            :me
            ((juxt :name :hobby :furry? :happy?))
            (= ["Henry" "curling" nil true]))
        "Fragments are matched"))

  (is (-> (round-trip
           (schema/query me
             {:root/xkeys [:name
                              [:NAME {:graphql/alias-of :name
                                      :other-param "prevents-overwrite"}]]}
             [])
           {:me {:name "Henry"
                 :NAME "HENRY"}})
          :me
          ((juxt :NAME :name))
          (= ["HENRY" "Henry"]))
      "Alias")

  (is (= (->> (round-trip
               (schema/query me
                 {:root/xkeys [[:pets
                                   :id
                                   [:... {:on "Pet"} :name]]]}
                 [])
               {:me {:pets [{:id "B"
                             :name "Bob"
                             :__typename "Pet"}
                            {:id "C"
                             :name "Candy"
                             :__typename "Pet"}]
                     :__typename "Person"}})
              :me
              :pets
              (mapv :name))
         ["Bob"
          "Candy"])
      "Plural fields")

  (is (-> (round-trip
           (schema/query me
             {:root/xkeys [[:label {:locale :$locale}]]
              :root/variables {:locale "en"}}
             [{locale :String}])
           {:me {:label "Breakfast"}})
          :me
          :label
          (= "Breakfast"))
      "Root query with parameters")

  (is (let [req (schema/query membership
                  {:root/xkeys [:id
                                   :unread-count]
                   :root/variables {:id 10
                                       :unread-count 1}}
                  [{id :String!}])
            data {:membership {:id 10
                               :unread-count 1}}]
        (-> (round-trip req data)
            :membership
            :unread-count
            (= 1)))
      "Root query parameters, nested key")

  (-> (root/read-root cache
                      (schema/query me {:root/xkeys [:name]} []))
      :me
      :name
      (= nil)
      (is "If entity does not exist, don't get keys from it."))

  (-> (round-trip (schema/query person
                    {:root/xkeys [[:pets :name]
                                     [:houses :address]]}
                    [])
                  {:person {:pets nil
                            :houses []}})
      :person
      ((juxt :pets :houses))
      (= [nil []])
      (is "Nil values and empty vectors handled appropriately"))

  (-> (round-trip
       (schema/mutation sessionEnd
         {:root/xkeys [[:... {:on :BooleanResponse}
                           :value
                           :message]]}
         [])
       {:sessionEnd {:value true, :message nil, :__typename "BooleanResponse"}, :__typename "Mutation"})
      :sessionEnd
      :value
      (= true)
      (is "Mutations work")))

