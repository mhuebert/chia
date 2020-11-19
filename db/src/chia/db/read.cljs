(ns chia.db.read
  (:refer-clojure :exclude [get get-in select-keys contains?])
  (:require [chia.db.core :as db]
            [chia.db.patterns :as patterns]
            [clojure.core :as core]
            [clojure.set :as set]))

(defn resolve-id
  "Returns id, resolving lookup refs (vectors of the form `[attribute value]`) to ids.
  Lookup refs are only supported for indexed attributes.
  The 3-arity version is for known lookup refs, and does not check for uniqueness."
  [db id]
  (when ^boolean (vector? id)
    (patterns/log-read db :_av id))
  (db/resolve-id @db id))

(defn contains?
  "Returns true if entity with given id exists in db."
  [db id]
  (let [id (resolve-id db id)]
    (when-not ^boolean (nil? id) (patterns/log-read db :e__ id))
    (true? (core/contains? (core/get @db :eav) id))))

(declare get entity)

(defn entity
  "Returns entity for resolved id."
  [db id]
  (when-let [id (resolve-id db id)]
    (patterns/log-read db :e__ id)
    (db/get-entity @db id)))

(defn get
  "Get attribute in entity with given id."
  ([db id attr]
   (get db id attr nil))
  ([db id attr not-found]
   (when-let [id (resolve-id db id)]
     (patterns/log-read db :ea_ [id attr])
     (core/get-in @db [:eav id attr] not-found))))

(defn get-in
  "Get-in the entity with given id."
  ([db id ks]
   (get-in db id ks nil))
  ([db id ks not-found]
   (when-let [id (resolve-id db id)]
     (patterns/log-read db :ea_ [id (first ks)])
     (core/get-in @db (into [:eav id] ks) not-found))))

(defn select-keys
  "Select keys from entity of id"
  [db id ks]
  (when-let [id (resolve-id db id)]
    (patterns/log-read db :ea_ (mapv #(do [id %]) ks) true)
    (-> (core/get-in @db [:eav id])
        (assoc :db/id id)
        (core/select-keys ks))))

(defn touch
  "Add refs to entity"
  [db {:keys [db/id] :as entity}]
  (reduce-kv
    (fn [m attr ids]
      (assoc m (keyword (core/namespace attr) (str "_" (name attr)))
               ids))
    entity
    (core/get-in @db [:vae id])))


(defn entity-ids
  [db qs]
  (assert (satisfies? IDeref db))
  (->> qs
       (mapv (fn [q]
               (set (cond (fn? q)
                          (reduce-kv (fn [s id entity] (if ^boolean (q entity) (conj s id) s)) #{} (core/get @db :eav))

                          (keyword? q)
                          (do (patterns/log-read db :_a_ q)
                              (reduce-kv (fn [s id entity] (if ^boolean (core/contains? entity q) (conj s id) s)) #{} (core/get @db :eav)))

                          :else
                          (let [[attr val] q
                                db-snap @db]
                            (patterns/log-read db :_av [attr val])
                            (if (db/index? db-snap attr)
                              (core/get-in db-snap [:ave attr val])
                              (entity-ids db [#(= val (core/get % attr))])))))))
       (apply set/intersection)))

(defn entities
  [db qs]
  (assert (satisfies? IDeref db))
  (map #(db/get-entity db %) (entity-ids db qs)))
