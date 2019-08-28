(ns chia.routing.query
  (:require [chia.db :as db]
            [chia.routing :as routing]
            [chia.routing.routers :refer [db-id]]
            [applied-science.js-interop :as j]))

(defn query-nav!
  "Navigates to current route with query-string replaced by the provided `query` map."
  [query]
  (let [params (reduce-kv (fn [params k v]
                            (doto ^js params
                              (.set (routing/encode k) (routing/encode v))))
                          (new routing/URLSearchParams)
                          (routing/remove-empty query))]
    (-> (routing/url (j/get js/location :href))
        (j/assoc! :search (str params))
        (str)
        (routing/nav!))))

(defn query-swap!
  "Navigates to current route with query parameters modified by `f`,
   which is passed the current query-map followed by `args`."
  [f & args]
  (-> (apply f (routing/query-from-string (routing/get-route)) args)
      (query-nav!)))

(defn get-query
  ([] (db/get db-id :query))
  ([k] (get-query k nil))
  ([k not-found]
   (db/get-in db-id [:query k] not-found)))

(defn assoc-query! [k v & kvs]
  (query-swap! merge (apply hash-map k v kvs)))

(defn update-query! [k f & args]
  (query-swap! update k (fn [v] (apply f v args))))
