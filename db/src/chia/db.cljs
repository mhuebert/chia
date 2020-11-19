(ns chia.db
  (:refer-clojure :exclude [get get-in contains? select-keys namespace])
  (:require [chia.db.core :as d]
            [chia.db.read :as read]
            [chia.db.macros :as m]))

(defonce ^:dynamic *db* (d/create {}))

(m/defpartial entity {:f '(read/entity *db* _)}
  [id])

(m/defpartial get {:f '(read/get *db* _)}
  ([id attr])
  ([id attr not-found]))

(m/defpartial get-in {:f '(read/get-in *db* _)}
  ([id path])
  ([id path not-found]))

(m/defpartial select-keys {:f '(read/select-keys *db* _)}
  [id ks])

(m/defpartial entity-ids {:f '(read/entity-ids *db* _)}
  [qs])

(m/defpartial entities {:f '(read/entities *db* _)}
  [qs])

(m/defpartial contains? {:f '(read/contains? *db* _)}
  [id])

(m/defpartial touch {:f '(read/touch *db* _)}
  [entity])

(m/defpartial transact! {:f '(d/transact! *db* _)}
  ([txs])
  ([txs opts]))

(def listen (partial d/listen *db*))
(def unlisten (partial d/unlisten *db*))
(def merge-schema! (partial d/merge-schema! *db*))

(def unique-id d/unique-id)
