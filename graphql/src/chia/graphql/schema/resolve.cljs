(ns chia.graphql.schema.resolve
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [chia.graphql.schema :as schema]
            [chia.graphql.types :as types]
            [chia.util :as u]
            [applied-science.js-interop :as j]
            [cljs.pprint :as pp]))

(declare type-key-terminal
         resolve-type-map)

(defn- enum-name [x]
  (if (number? x)
    x
    (name x)))

(defn registered-entry [registry k]
  (or (get registry k)
      (throw (js/Error. (str "Key not registered: " k)))))

(defn- fields->js
  "Returns js-formatted type map for GraphQL constructors"
  [registry entries]
  (->> entries
       (reduce-kv (fn [obj k v]
                    (j/assoc! obj
                              (name k)
                              (-> (resolve-type-map registry v)
                                  (clj->js)))) #js {})))

(def ^:private type-map-terminal
  (-> (fn [registry {:as   type-map
                     :keys [schema/type-key
                            terminal]}]
        (if-let [constructor (some-> terminal
                                     (types/builtin))]
          (new constructor
               (-> (case terminal
                     :Union (-> type-map
                                (set/rename-keys {:data :types})
                                (update :types #(->> (mapv (partial type-key-terminal registry) %)
                                                     (to-array))))
                     :Enum (-> type-map
                               (set/rename-keys {:data :values})
                               (update :values #(->> (mapv enum-name %)
                                                     (reduce
                                                      (fn [m enum]
                                                        (j/assoc! m enum #js {:value enum})) #js {}))))
                     (:Object
                      :InputObject
                      :Interface) (-> type-map
                                      (update :interfaces #(some->> (mapv (partial type-key-terminal registry) %)
                                                                    (to-array)))
                                      (assoc :fields #(->> (select-keys registry (:field-keys type-map))
                                                           (fields->js registry)))))
                   (dissoc :schema/type-key)
                   (assoc :name (name type-key))
                   (clj->js)))
          (if-let [scalar (types/scalar type-key)]
            scalar
            (:type (resolve-type-map registry (registered-entry registry type-key))))))
      (u/memoize-by
       (fn [[_ type-map]] (select-keys type-map [:schema/type-key
                                                 :args
                                                 :data
                                                 :field-keys])))))

(defn resolve-type-map [registry {type-key :schema/type-key
                                  :as      type-map}]
  (let [{:keys [schema/base-key
                schema/is-list?
                schema/required-outer?
                schema/required-inner?]} (schema/parse-type-key type-key)
        the-type (cond-> (type-map-terminal registry (assoc type-map :schema/type-key base-key))
                         required-inner? (types/NonNull)
                         is-list? (types/List)
                         required-outer? (types/NonNull))]
    (-> type-map
        (assoc :type the-type)
        (cond-> (:args type-map)
                (update :args (fn [args]
                                (u/update-vals args #(resolve-type-map registry (schema/as-type-map %)))))))))

(defn type-key-terminal [registry k]
  (->> (schema/as-type-map k)
       (resolve-type-map registry)
       :type))

(defn select-root [registry]
  (-> registry
      (select-keys [:Query
                    :Mutation
                    :Subscription])
      (u/update-map (comp str/lower-case name)
                    :type)
      (assoc :types (:types registry))))

(defn resolve-js-types [{:as   registry
                         :keys [:schema/implementors]}]
  (-> registry
      (dissoc :schema/implementors)
      (u/update-vals (partial resolve-type-map registry))
      (as-> registry
            (let [implementors (u/update-vals implementors #(mapv (partial type-key-terminal registry) %))
                  types (reduce into #{} (vals implementors))]
              (assoc registry :schema/implementors implementors
                              :types types)))
      (select-root)))

(defn js-schema
  "Return a graphql-js Schema for the given fields."
  ([] (js-schema {}))
  ([options] (js-schema @schema/*schema-ref* options))
  ([registry {:as   options
              :keys [resolvers
                     wrap-resolver]}]
   (new types/Schema
        (-> registry
            (schema/register-resolvers resolvers)
            (cond-> wrap-resolver
                    (u/update-vals #(u/update-some-keys % [:resolve] wrap-resolver)))
            (resolve-js-types)
            (clj->js)))))


(comment

 (defn get-field [parent k]
   (if (satisfies? ILookup parent)
     (get parent (keyword k))
     (j/get parent k)))

 (defn default-field-resolver [[parent params context ^js info]]
   (get-field parent (.-fieldName info)))

 (defn resolve-data [registry data params context]
   ;; NOTE
   ;; we _do_ need to take into account the current fragment/query here --
   ;; because we only want to resolve keys that have been requested.

   ;; resolve - [parent params context info]
   (let [type-key (get-field data :__typename)
         ks (get-in registry [type-key :field-keys])]
     (u/for-map [k ks
                 :let [k-map (get registry k)
                       resolver (or (get-in registry [k :resolve])
                                    (when-let [interface (get-in registry [k :of-interface])]
                                      (get-in registry [interface k :resolve]))
                                    default-field-resolver)]]
       {(schema/typekey k) (resolver [data params context #js {:fieldName (name k)}])}))))


