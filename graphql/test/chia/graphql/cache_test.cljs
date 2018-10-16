(ns chia.graphql.cache-test
  (:require [cljs.test :as test :refer [deftest is are testing]]
            [chia.graphql.cache :as cache]
            [chia.graphql.normalize :as n]
            [chia.graphql :as g]
            [chia.triple-db.core :as d]))

(g/def ^:Fragment parent-fields {:on :Human}
  :id :name)

(g/def ^:Fragment pet-fields {:on :Pet}
  :id :name)

(g/def ^:Fragment human-fields {:on :Entity}
  :id
  [:father
   parent-fields]
  :name
  [:NAME {:gql/alias-of :name}]

  [:photo {:width 10
           :height 10}]
  [:pets pet-fields])

(g/def ^:Fragment human {:on :Entity}
  human-fields)

(g/defn ^:Query get-human []
  human-fields)

(def cache
  (d/create))

(defn round-trip [query data]
  (-> (d/create)
      (n/cache-response! {:query query}
                         (clj->js {:data data}))
      (n/read-query {:query query})))

(test/deftest graphql-cache

  (is (-> (round-trip (g/fn ^:Query []
                            :name)
                      {:name "Henry"})
          :name
          (= "Henry"))
      "Simple key")

  (let [cache (d/create)
        _ (n/cache-response! cache
                             {:query (g/fn ^:Query [] :id :name)}
                             #js {:data #js {:id "A"
                                             :name "Henry"}})]
    (is (-> (n/read-keys cache "A" :name)
            :name
            (= "Henry"))
        "Data is normalized by id"))

  (let [record-fragment (g/fragment {:on "Person"} :name)
        inline-fragment [:... {:on "Person"} :hobby]]
    (is (-> (round-trip (g/fn ^:Query []
                              record-fragment
                              inline-fragment)
                        {:name "Henry"
                         :hobby "curling"})
            ((juxt :name :hobby))
            (= ["Henry" "curling"]))
        "Fragments"))

  (is (-> (round-trip (g/fn ^:Query []
                            :name
                            [:NAME {:gql/alias-of :name
                                    :other-param "prevents-overwrite"}])
                      {:name "Henry"
                       :NAME "HENRY"})
          ((juxt :NAME :name))
          (= ["HENRY" "Henry"]))
      "Alias")

  (is (= (->> (round-trip (g/fn ^:Query []
                                [:pets :id
                                 [:... {:on "Person"} :name]])
                          {:pets [{:id "B"
                                   :name "Bob"}
                                  {:id "C"
                                   :name "Candy"}]
                           :__typename "Person"})
              :pets
              (mapv :name))
         ["Bob"
          "Candy"])
      "Plural fields"))

