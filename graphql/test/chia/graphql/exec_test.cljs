(ns chia.graphql.exec-test
  (:require [chia.graphql.schema :as schema]
            [chia.graphql.exec :as exec]
            [clojure.test :as t :refer [is]]
            [kitchen-async.promise :as p]
            [cljs.pprint :as pp]
            [chia.graphql.root :as root]))

(comment
 (t/deftest register-resolvers

   (schema/object ::Board
     {:title       :String!
      :description :String})

   (schema/object ::Group
     {:title       :String!
      :description :String})

   (schema/union ::Entity
     #{::Board
       ::Group})

   (schema/defquery entity
     [{:id :Int!}]
     ::Entity)

   (schema/register-resolvers!
    {entity  (fn [[parent {:as   vars
                           :keys [id]} context info]]
               {:description     (str "Entity of id " id)
                :schema/type-key (case id 99 ::Board
                                          100 ::Group)})
     ::Board {:title (fn [[parent variables context info]]
                       {:title "Board Title"})}
     ::Group {:title (constantly "Group Title")}})

   (doseq [entry-key [entity
                      [::Board :title]]]
     (-> (schema/entry entry-key)
         :resolve
         (fn?)
         (is (str "Resolver for " entry-key " is registered."))))


   (root/exec! entity {:id 99}
     :title
     :description
     :schema/type-key)
   #_(root/exec! root)


   (t/async done
     (p/let [response (root/exec! entity {:id 99}
                        :schema/type-key
                        :title
                        :description)]
       (pp/pprint [:response 99 response])
       (is (= (:schema/type-key response) ::Board))
       (done)))

   (t/async done
     (p/let [response (root/exec! entity {:id 100}
                        :schema/type-key
                        :title
                        :description)]
       (pp/pprint [:response 100 response])
       (is (= (:schema/type-key response) ::Group))
       (done)))))