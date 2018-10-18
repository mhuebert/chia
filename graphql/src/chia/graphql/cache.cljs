(ns chia.graphql.cache
  (:require [chia.triple-db.core :as d]
            [chia.graphql :as g]
            [chia.graphql.normalize :as n]
            [chia.view :as v]
            [chia.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GraphQL Cache

(defonce cache (d/create))

(def read-query (partial n/read-query cache))

(defn merge-query-meta! [req attrs]
  (d/transact! cache [(assoc attrs
                        :db/id (n/req-id req))]))

(defn remove-query-root! [req]
  (d/transact! cache [[:db/retract-entity (n/req-id req)]]))

(defn write-response! [req response]
  (n/cache-response! cache req response)
  (n/read-query cache req))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keep track of view<>req relationships

(defonce ^:private req-listeners (volatile! {}))

(defn ^:private unmount-req! [view req]
  (vswap! req-listeners update-in [req :views] disj view)
  (-> #(when (some-> (get-in @req-listeners [req :views])
                     (u/guard set?)
                     (empty?))
         (doseq [on-unmount (vals (get-in @req-listeners [req :on-unmount] req))]
           (on-unmount req))
         (vswap! req-listeners dissoc req))
      ;; wait before unmounting to avoid rapid unmount/mount cycles during page transitions
      (js/setTimeout 500)))

(defn on-unmount!
  "Register a callback to be evaluated when `req` is no longer listened to by any views."
  [req k f]
  (vswap! req-listeners assoc-in [req :on-unmount k] f))

(defn listen! [{:as req
                :keys [query]}]
  (let [mount-req? (empty? (get-in @req-listeners [req :views]))
        current-view v/*current-view*]
    (vswap! req-listeners update-in [req :views] (fnil conj #{}) current-view)
    (v/on-unmount! current-view req #(unmount-req! current-view req))
    (when mount-req?
      (let [{:keys [gql/on-mount
                    gql/on-unmount]} (g/options query)]
        (when on-unmount
          (on-unmount! req :lifecycle on-unmount))
        (when on-mount
          (on-mount req)))
      true)))