(ns chia.routing.resolve
  (:require [chia.routing.registry :as registry]
            [kitchen-async.promise :as p]
            [bidi.bidi :as bidi]
            [clojure.set :as set]
            [chia.routing.routers :refer [db-id]]
            [chia.db :as db]
            [chia.lazy :as lazy]
            [cljs.pprint :as pp]))

(defn resolve-path [router path]
  (-> (or (bidi/match-route (registry/paths router) path)
          {:handler :handler/not-found})
      (set/rename-keys {:handler :handler-key
                        :tag :handler-tag})))

(defn resolve-routes
  "Given a map of {<router-key>, <path-or-data>}, matches paths to handlers."
  [router-paths]
  (p/->> router-paths
         (mapv (fn [[router-key path]]
                 (if (= :query router-key)
                   [router-key path]
                   (let [{:as match
                          :keys [handler-key]} (resolve-path router-key path)]
                     (p/let [view (or (registry/handler handler-key)
                                      (do (println :warn "missing handler: " handler-key)
                                          (registry/handler :handler/not-found)))]
                       [router-key
                        (merge match
                               {:view view
                                :router router-key
                                :path path})])))))
         (p/all)
         (into {})))

(defn resolve-async [routers]
  (let [updates (lazy/checked
                  (p/let [resolved-routers (resolve-routes routers)]
                    (db/transact! [[:db/add db-id :async/loading? false]])
                    resolved-routers))]
    (p/do (p/timeout 50)
          (when-not (lazy/loaded? updates)
            (db/transact! [[:db/add db-id :async/loading? true]])))
    updates))