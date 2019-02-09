(ns chia.graphql.schema
  (:refer-clojure :exclude [object?])
  (:require [chia.util :as u]
            [chia.util.macros :as m]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [clojure.pprint :as pp])
  #?(:cljs (:require-macros [chia.graphql.schema])))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Util

(defn join-keywords
  "Joins two keywords, moving all segments into `namespace` except `child` name.

  [:a.b/c, :d/e] => :a.b.c.d/e"
  [parent child]
  (keyword (str (some-> (namespace parent) (str "."))
                (name parent)
                (some-> (namespace child)
                        (str "."))) (name child)))

(defn keyword-append
  "Appends `suffix` to name of keyword."
  [k suffix]
  (keyword (namespace k) (str (name k) suffix)))

(defn- flatten-nested-keywords [m]
  (reduce-kv (fn [m k v]
               (if (map? v)
                 (reduce-kv (fn [m child-k v]
                              (case child-k
                                :_ (assoc m k v)
                                (assoc m (join-keywords k child-k) v))) m v)
                 (assoc m k v))) {} m))

(s/fdef flatten-nested-keywords
        :args (s/cat :map
                     (s/map-of keyword?
                               (s/or :map (s/map-of keyword? any?)
                                     :terminal (complement map?))))
        :ret (s/map-of keyword? (complement coll?)))

(def conj-set (fnil conj #{}))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Registry

(defonce ^:dynamic *schema-ref* (atom {:schema/implementors {}}))

(def scalar-types #{:String
                    :Int
                    :Float
                    :Boolean
                    :ID})

(def abstract-types #{:Union
                      :Interface})

(def object-types #{:Object
                    :InputObject})

(def modifier-types #{:NonNull
                      :List})

(def built-ins
  (conj (set/union scalar-types
                   abstract-types
                   object-types
                   modifier-types)
        :Enum
        :Schema))

(def concrete-types
  (conj scalar-types :Enum))

(defn registered-type? [k]
  (or (contains? built-ins k)
      (contains? @*schema-ref* k)))


(defn parse-type-key [type-key]
  (let [[List? k] (if (vector? type-key)
                    [true (first type-key)]
                    [false type-key])
        required-outer? (and List?
                             (:required (meta type-key)))
        [base-key required-inner?] (let [[_ kw-name required-inner?] (re-find #"([^!]+)(!)?$" (name k))]
                                     [(if required-inner?
                                        (keyword (namespace k) kw-name)
                                        k)
                                      required-inner?])]
    #:schema{:type-key        type-key
             :base-key        base-key
             :is-list?        List?
             :required-outer? required-outer?
             :required-inner? required-inner?}))

(def get-base (comp :schema/base-key parse-type-key))

(def concrete?
  (comp
   (fn [k]
     (or (concrete-types k)
         (-> @*schema-ref*
             :schema/scalars
             (contains? k))))
   get-base))

(def object? (comp object-types
                   get-base))

(def ambiguous? (comp abstract-types
                      get-base))

(def is-list? (comp :schema/is-list?
                    parse-type-key))

(def built-in? #(contains? built-ins %))

(defn resolve-runtime-type
  [return-key value]
  (cond (ambiguous? return-key)
        (let [resolve-type (get-in @*schema-ref* [(get-base return-key) :type-key-resolver] :schema/type-key)
              concrete-key (resolve-type value)]
          (assert (some? concrete-key)
                  (str "Type `" return-key "` is ambiguous and could not be resolved. Result should contain :type-key or :type-key-resolver should be provided."))
          [concrete-key (cond-> value
                                (object? concrete-key) (assoc :schema/type-key concrete-key))])
        (object? return-key)
        [return-key (some-> value
                            (assoc :schema/type-key return-key))]
        :else
        (do
          (assert (concrete? return-key))
          [return-key value])))

(defn entry
  ([k]
   (entry @*schema-ref* k))
  ([registry k]
   (assert (not (vector? k)))
   (get registry (get k :schema/type-key k))
   #_(if (vector? k)
       (entry registry (apply join-keywords k))
       (get registry (get k :schema/type-key k)))))

(defn resolve-base-key [type-key]
  (let [{:keys [schema/base-key]} (parse-type-key type-key)]
    (if (built-in? base-key)
      base-key
      (let [{:as   type-map
             :keys [terminal
                    schema/type-key]} (entry base-key)]
        (cond terminal terminal
              (built-in? type-key) type-key
              (nil? type-key) nil
              :else (base-key type-key))))))

(defn unwrap-vec [x]
  (cond-> x
          (vector? x) (first)))

(defn entry-at-path [path]
  (loop [k (entry (unwrap-vec (first path)))
         remaining (rest path)]
    (cond (empty? remaining) k
          ;; maps are used as lookups in lists, eg. [:people {:id 1} :name]
          (map? (first remaining)) (recur k (rest remaining))
          :else
          (recur (entry (join-keywords (unwrap-vec (:schema/type-key k))
                                       (unwrap-vec (first remaining))))
                 (rest remaining)))))

(def key-at-path (comp #(get % :schema/type-key) entry-at-path))

(def base-key-at-path (comp resolve-base-key key-at-path))

(comment
 (def path-keys (juxt key-at-path base-key-at-path))
 (assert (= (path-keys [:spark-gql.schema.entities/Board :id])
            [:String! :String]))
 (assert (= (path-keys [:spark-gql.schema.entities/Board :actions :view :public])
            [:Boolean! :Boolean]))
 (assert (= (path-keys [:spark-gql.schema.entities/Board :membership :roles])
            [[:spark-gql.schema.accounts/Role] :Enum]))
 (assert (= (path-keys [:spark-gql.schema.entities/Board :images :logo])
            [:String :String])))

(defn- as-type-map [t]
  (cond (map? t) t
        (or (keyword? t)
            (vector? t)) {:schema/type-key t}
        :else (throw (ex-info (str "Invalid type: " t) {:type t}))))

(defn make-child-entries
  ([object-typespace data]
   (make-child-entries {} object-typespace data))
  ([registry parent-key data]
   (->> data
        (reduce-kv
         (fn [m child-key v]
           (if (and (namespace child-key) (map? v))
             (make-child-entries m parent-key
                                 (u/update-vals v (comp #(assoc % :of-interface child-key) as-type-map)))
             (let [{:as   child-map
                    :keys [of-interface]} (as-type-map v)
                   child-key (join-keywords parent-key child-key) #_(keyword (name parent-key) (name child-key))]
               (-> m
                   ;; add
                   (assoc child-key child-map)
                   (update-in [parent-key :field-keys] conj-set child-key)
                   (cond-> of-interface
                           (-> (update-in [parent-key :interfaces]
                                          conj-set of-interface)
                               (update-in [:schema/implementors of-interface]
                                          conj-set parent-key)))))))
         registry))))

(defn- type-with-children [parent-key]
  (fn [registry {:as   type-map
                 :keys [schema/type-key]}]
    (-> registry
        (update type-key
                (fnil merge {})
                (-> type-map
                    (assoc :terminal parent-key)
                    (dissoc :data :nil)))
        (make-child-entries type-key (:data type-map)))))

(defn- union-entry [registry {:as   type-map
                              :keys [schema/type-key]
                              types :data}]
  {:pre [(set? types)]}
  (-> registry
      (assoc type-key (-> type-map
                          (assoc :terminal :Union)))
      (update-in [:schema/implementors type-key] (fnil into #{}) types)))

(defn- enum-entry [registry {:as   type-map
                             :keys [schema/type-key]}]
  {:pre [(set? (:data type-map))]}
  (assoc registry type-key
                  (-> type-map
                      (assoc :terminal :Enum))))

(defn- register-type [registry entry-f type-map]
  (entry-f registry type-map))

(s/def ::def-args (s/cat :schema/type-key keyword?
                         :description (s/? string?)
                         :data any?
                         :nil (s/? nil?)))

(defn- args->map [args]
  {:post [(not (s/invalid? %))]}
  (s/conform ::def-args args))

(def implementors-simple
  (memoize
   (fn [{:as   registry
         :keys [schema/implementors]}]
     (u/update-map implementors name #(into #{} (map name) %)))))

(defn ^:dynamic *fragment-matches?* [data-type fragment-type]
  (or (nil? fragment-type)
      (= data-type fragment-type)
      (some-> (implementors-simple @*schema-ref*)
              (get fragment-type)
              (contains? data-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Types API

(defn object [key & [description fields :as args]]
  "Registers a GraphQL Object type."
  (swap! *schema-ref* register-type
         (type-with-children :Object)
         (args->map (cons key args)))
  key)

(defn input [key & [description fields :as args]]
  "Registers a GraphQL InputObject type."
  (swap! *schema-ref* register-type
         (type-with-children :InputObject)
         (args->map (cons key args)))
  key)

(defn interface [key & [description fields :as args]]
  "Registers a GraphQL Interface type."
  (swap! *schema-ref* register-type
         (type-with-children :Interface)
         (args->map (cons key args)))
  key)

(defn union [key & [description type-keys :as args]]
  "Registers a GraphQL Union type."
  (swap! *schema-ref* register-type
         union-entry
         (args->map (cons key args)))
  key)

(defn enum [key & [description values :as args]]
  "Registers a GraphQL Enum type."
  (swap! *schema-ref* register-type
         enum-entry
         (args->map (cons key args)))
  key)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Type modifier API

(defn args
  "Adds arguments to a type

   {:imageUrl (args :String
                {:width :Int})}"
  [t args]
  (assoc (as-type-map t) :args args))

(defn description
  "Adds a description string to a type

   {:postalCode (description :String
                  'Should not contain spaces'}"
  [t description*]
  (assoc (as-type-map t) :description description*))

(defn props
  ([t props]
   (assoc (as-type-map t) :props props))
  ([t k1 & kvs]
   (props t (apply hash-map (cons k1 kvs)))))

(defn !
  "Indicates that a type is required. Especially useful for List types.

   (! [:String])"
  [t]
  (cond (vector? t) (with-meta t {:required true})
        (keyword? t) (keyword-append t "!")
        (map? t) (assoc t :required true)))

(defrecord Root [])

;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resolver API

(defn register-resolvers
  [registry resolvers]
  (-> (or resolvers {})
      (u/update-keys #(get % :schema/type-key %))
      (flatten-nested-keywords)
      (->> (reduce-kv (fn [registry type-key resolver]
                        (assert (contains? registry type-key)
                                (str "Attempted to register a resolver for non-existent key: " type-key))
                        (assoc-in registry [type-key :resolve] resolver)) registry))))

(defn register-resolvers!
  [resolvers]
  (swap! *schema-ref* register-resolvers resolvers))

;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros

(defn field* [operation type-map root-options]
  (let [object-k (object operation {(keyword (:name type-map)) type-map})]
    (map->Root (merge
                {:schema/type-key (join-keywords object-k (keyword (:name type-map)))
                 :root/variables  {}
                 :root/options    {}}
                root-options))))

(m/defn ^:private field [operation args]
  (let [parsed-args
        (s/conform (s/cat :name symbol?
                          :description (s/? string?)
                          :options (s/? map?)
                          :arglist vector?
                          :schema/type-key keyword?) args)

        type-map (-> parsed-args
                     (update :name name)
                     (update :arglist (fn [arglist] (or (some-> (first arglist)
                                                                (u/update-keys keyword))
                                                        {})))
                     (set/rename-keys {:arglist :args})
                     (dissoc :options))]
    `(field* ~operation
             ~type-map
             ~(:options parsed-args))))

(m/defmacro query [name & [description options arglist return-type :as args]]
  (field :Query (cons name args)))

(m/defmacro mutation [name & [description options arglist return-type :as args]]
  (field :Mutation (cons name args)))

(m/defmacro subscription [name & [description options arglist return-type :as args]]
  (field :Subscription (cons name args)))

(m/defmacro defquery [name & [description options arglist return-type :as args]]
  `(def ~name ~(field :Query (cons name args))))

(m/defmacro defmutation [name & [description options arglist return-type :as args]]
  `(def ~name ~(field :Mutation (cons name args))))

(m/defmacro defsubscription [name & [description options arglist return-type :as args]]
  `(def ~name ~(field :Subscription (cons name args))))