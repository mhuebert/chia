(ns chia.graphql.schema.resolve
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [chia.graphql.schema :as schema]
            [chia.graphql.types :as types]
            [chia.util :as u]
            [chia.util.js-interop :as j]
            [clojure.spec.alpha :as s]))

(declare type-key-terminal resolve-type-map)

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
                              (schema/typename k)
                              (-> (resolve-type-map registry v)
                                  (clj->js)))) #js {})))

(def ^:private type-map-terminal
  (u/memoize-by
   (fn [registry {:as type-map
                  :keys [type-key]}]

     (if-let [constructor (types/constructors type-key)]
       (new constructor
            (-> (case type-key
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
                (dissoc :type-key)
                (clj->js)))
       (if-let [primitive (types/primitive type-key)]
         primitive
         (:type (resolve-type-map registry (registered-entry registry type-key))))))
   (fn [[_ type-map]] (select-keys type-map [:type-key
                                             :args
                                             :data
                                             :field-keys]))))

(defn resolve-type-map [registry {type-key :type-key
                                  :as type-map}]
  (let [[List? k] (if (vector? type-key)
                    [true (first type-key)]
                    [false type-key])
        required-outer? (and List?
                             (:required (meta type-key)))
        [kw required-inner?] (let [[_ kw-name required-inner?] (re-find #"([^!]+)(!)?$" (name k))]
                               [(if required-inner?
                                  (keyword (namespace k) kw-name)
                                  k)
                                required-inner?])
        the-type (cond-> (type-map-terminal registry (assoc type-map :type-key kw))
                         required-inner? (types/NonNull)
                         List? (types/List)
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

(defn resolve-types [{:as registry
                      :keys [:schema/implementors]}]
  (-> registry
      (dissoc :schema/implementors)
      (u/update-vals (partial resolve-type-map registry))
      (as-> registry
            (let [implementors (u/update-vals implementors #(mapv (partial type-key-terminal registry) %))
                  types (reduce into #{} (vals implementors))]
              (assoc registry :schema/implementors implementors
                              :types types)))))

(defn register-root [registry fields]
  (->> ["query" "mutation" "subscription"]
       (reduce (fn [registry field-name]
                 (let [fields (some-> (get fields (keyword field-name))
                                      (u/update-vals :schema/type-map))
                       make-entry (schema/fields-entry :Object)]
                   (cond-> registry
                           fields (make-entry
                                   (schema/args->map (keyword (str/capitalize field-name))
                                                     fields)))))
               registry)))

(defn- flatten-resolver-map [m]
  (reduce-kv (fn [m k v]
               (if (map? v)
                 (reduce-kv (fn [m child-k v]
                              (assoc m (schema/inherit-typespace child-k k) v)) m v)
                 (assoc m k v))) {} m))

(defn with-resolvers [registry resolvers]
  (->> resolvers
       (reduce-kv (fn [m k v]
                    (when-not (get m k)
                      (prn :NO_REGISTRY_VALUE k v))
                    (assoc-in m [k :resolve] v)) registry)))

(defn get-top-level [registry]
  (-> registry
      (select-keys [:Query
                    :Mutation
                    :Subscription])
      (u/update-map (comp str/lower-case name)
                    :type)
      (assoc :types (:types registry))))

(defn make-schema
  "Return a graphql-js Schema for the given fields."
  [{:as fields
    :keys [registry
           resolvers
           wrap-resolver]
    :or {registry @schema/*registry-ref*}}]

  (-> (register-root registry fields)
      (with-resolvers (some-> resolvers
                              (flatten-resolver-map)
                              (u/update-vals wrap-resolver)))
      (resolve-types)
      (get-top-level)
      (clj->js)
      (->> (new types/Schema))))

(s/def ::registry
  (s/map-of keyword? (s/or :type-map ::schema/type-map
                           :root #(instance? schema/Root %))))

(s/fdef make-schema
        :args (s/cat :fields (s/keys
                              :opt-un [::registry])))

;;;;;;;;;;;;;;;;;;;
;;
;;

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
                       resolver (or (get k-map :resolver)
                                    (when-let [i-key (:implementation-of k-map)]
                                      (get-in registry [i-key :resolver]))
                                    default-field-resolver)]]
       {(schema/typekey k) (resolver [data params context #js {:fieldName (schema/typename k)}])}))))

(s/def ::terminal-type types/type?)

(s/fdef type-map-terminal
        :ret ::terminal-type)
(s/fdef type-key-terminal
        :ret ::terminal-type)
