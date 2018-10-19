(ns chia.graphql.cache-test
  (:require [cljs.test :as test :refer [deftest is are testing]]
            [chia.graphql.cache :as cache]
            [chia.graphql.normalize :as n]
            [chia.graphql :as g]
            [chia.util :as u]
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

(defn with-req [req data]
  (-> (d/create)
      (n/cache-response! req
                         (clj->js {:data data}))))

(defn round-trip [req data]
  (let [req (if (map? req) req
                           {:query req})
        cache (with-req req data)]
    (-> (n/read-query cache req)
        :async/value)))


(test/deftest graphql-cache

  (is (-> (round-trip (g/fn ^:Query []
                        :name)
                      {:name "Henry"})
          :name
          (= "Henry"))
      "Simple key")

  (let [cache (d/create)
        _ (n/cache-response! cache
                             {:query (g/fn ^:Query []
                                       :id :name
                                       [:pets
                                        :id :name])}
                             (clj->js {:data {:id "A"
                                              :name "Henry"
                                              :pets [{:id "B"
                                                      :name "Bertrand"}]}}))
        result (n/read-keys cache {:id "A"}
                            :name
                            [:pets :name])]
    (is (and (= (:name result) "Henry")
             (= (get-in result [:pets 0 :name]) "Bertrand"))
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
      "Plural fields")

  (is (-> (round-trip {:query (g/fn ^:Query [{:keys [^string locale]}]
                                [:label {:locale locale}])
                       :variables {:locale "en"}}
                      {:label "Breakfast"})
          :label
          (= "Breakfast"))
      "Root query with parameters")

  (is (let [req {:query (g/fn ^:Query [{:keys [^String! id]
                                        :as params}]
                          [:membership @params
                           :id
                           :unread-count])
                 :variables {:id 10}}
            data {:membership {:id 10
                               :unread-count 1}}]
        (-> (round-trip req data)
            :membership
            :unread-count
            (= 1)))
      "Root query parameters, nested key"))

