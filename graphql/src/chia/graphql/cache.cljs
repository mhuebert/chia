(ns chia.graphql.cache
  (:require [chia.triple-db.core :as d]
            [chia.graphql :as g]
            [chia.graphql.normalize :as n]
            [cljs.pprint :refer [pprint]]
            [chia.util.js-interop :as j]))

(def cache (d/create))

(def query-listeners (volatile! {}))
(def query-unmount (volatile! {}))

(defn merge-query-meta! [req attrs]
  (d/transact! cache [(assoc attrs
                        :db/id (n/req-id req))]))

(defn remove-query-root! [req]
  (d/transact! cache [[:db/retract-entity (n/req-id req)]]))

(defn write-response! [req response]
  (n/cache-response! cache req response)
  (n/read-query cache req))

(defn start-watch! [{:keys [current-view
                            query
                            variables
                            callbacks
                            refetch!]}]
  (let [start? (empty? (get @query-listeners [query variables]))]
    (when start?
      (let [{:keys [start end]} (if (fn? callbacks)
                                  (callbacks variables refetch!)
                                  callbacks)]
        (vswap! query-unmount assoc [query variables] end)
        (start)))
    (vswap! query-listeners update [query variables] (fnil conj #{}) current-view)
    start?))

(defn end-watch! [view path]
  (vswap! query-listeners update path disj view)
  (when-let [end (and (empty? (get @query-listeners path))
                      (get @query-unmount path))]
    (end)))