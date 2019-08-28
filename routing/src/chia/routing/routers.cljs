(ns chia.routing.routers
  (:require [chia.db :as db]))

(def db-id :db/routers)

(defn get-router
  ([]
   (db/entity db-id))
  ([k]
   ;; keep :routers here?
   (db/get db-id k)))