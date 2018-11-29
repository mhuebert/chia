(ns chia.graphql.schema
  (:require [chia.util :as u]
            [chia.util.macros :as m]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [expound.alpha :as expound])
  #?(:cljs (:require-macros [chia.graphql.schema])))

(defonce ^:dynamic *registry-ref* (atom {}))

(def built-ins #{:String
                 :Int
                 :Float
                 :Boolean
                 :ID
                 :NonNull
                 :Object
                 :InputObject
                 :Interface
                 :Union
                 :Enum
                 :List
                 :Schema})


(defn registered-type? [k]
  (or (contains? built-ins k)
      (contains? @*registry-ref* k)))

(defn get-key [k]
  (get @*registry-ref* k))

(defn- typespace [k]
  (keyword (namespace k)
           (re-find #"^[^.]+" (name k))))

(defn- typename [k]
  (re-find #"[^.]+$" (name k)))

(defn- typekey [k]
  (some-> (typename k)
          (keyword)))

(defn- keyword-append [k s]
  (keyword (namespace k)
           (str (name k) s)))

(defn- typekey-append [typespace k]
  (keyword-append typespace (str "." (name k))))

(defn- inherit-typespace
  "Adds/replaces typespace of `child-k`"
  [child-k typespace]
  (typekey-append typespace (typekey child-k)))

(defn- external-keys
  "Keys which cannot inherit from parent typespace (g. interfaces)"
  [operation ks]
  (-> (into #{} (comp (filter namespace)
                      (map typespace)) ks)
      (disj operation)))

(defn- external? [child-k object-typespace]
  (and (namespace child-k)
       (not= (typespace child-k)
             object-typespace)))


(s/fdef typespace
        :args (s/cat :key keyword?))

(s/fdef typekey
        :args (s/cat :key keyword?))

#_(s/fdef external?
          :args)

(defn- as-type-map [t]
  (cond (map? t) t
        (or (keyword? t)
            (vector? t)) {:type-key t}
        :else (throw (ex-info (str "Invalid type: " t) {:type t}))))

(defn field-entries
  ([object-typespace data]
   (field-entries {} object-typespace data))
  ([m object-typespace data]
   (->> data
        (reduce-kv
         (fn [entries child-k v]
           (assoc entries (inherit-typespace child-k object-typespace)
                          (cond-> (as-type-map v)
                                  (external? child-k object-typespace) (assoc :implementation-of child-k))))
         m))))

(defn- fields-entry [parent-key]
  (fn [registry {:keys [type-key
                        data]
                 description :doc}]
    (let [object-typespace (typespace type-key)
          fields (field-entries object-typespace data)
          field-keys (set (keys fields))
          interface-ks (external-keys object-typespace (keys data))
          parent-entry {:name (typename type-key)
                        :type-key parent-key
                        :description description
                        :interfaces interface-ks
                        :field-keys field-keys}]
      (-> registry
          (merge fields)
          (update :schema/implementors (fn [m]
                                         (->> interface-ks
                                              (reduce
                                               (fn [m interface-k]
                                                 (update m interface-k (fnil conj #{}) type-key)) m))))
          (assoc type-key parent-entry)))))

(defn- union-entry [m {:as type-map
                       :keys [type-key
                              doc]
                       types :data}]
  {:pre [(vector? types)]}
  (-> m
      (assoc type-key (-> type-map
                          (assoc :type-key :Union)))
      (update-in [:schema/implementors type-key] (fnil into #{}) types)))

(defn- enum-entry [m {:as type-map
                      :keys [type-key]}]
  {:pre [(vector? (:data type-map))]}
  (assoc m type-key
           (-> type-map
               (assoc :type-key :Enum))))

(defn- register-type [registry entry-f type-map]
  (entry-f registry type-map))

(defn- args->map [type-key & [description data]]
  {:pre [type-key]}
  (let [[description data] (if (string? description)
                             [description data]
                             [nil description])]
    {:name (typename type-key)
     :type-key type-key
     :description description
     :data data}))

(def implementors-simple
  (memoize
   (fn [{:as registry
         :keys [schema/implementors]}]
     (u/update-map implementors typename #(into #{} (map typename) %)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Public API for defining types

(defn object [key & [doc fields]]
  "Registers a GraphQL Object type."
  (swap! *registry-ref* register-type
         (fields-entry :Object)
         (args->map key doc fields))
  key)

(defn input [key & [doc fields]]
  "Registers a GraphQL InputObject type."
  (swap! *registry-ref* register-type
         (fields-entry :InputObject)
         (args->map key doc fields))
  key)

(defn interface [key & [doc fields]]
  "Registers a GraphQL Interface type."
  (swap! *registry-ref* register-type
         (fields-entry :Interface)
         (args->map key doc fields))
  key)

(defn union [key & [doc type-keys]]
  "Registers a GraphQL Union type."
  (swap! *registry-ref* register-type
         union-entry
         (args->map key doc type-keys))
  key)

(defn enum [key & [doc values]]
  "Registers a GraphQL Enum type."
  (swap! *registry-ref* register-type
         enum-entry
         (args->map key doc values))
  key)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Public API for type modifiers

(defn args
  "Adds arguments to a type

   {:imageUrl (args :String
                {:width :Int})}"
  [t args]
  (assoc (as-type-map t) :args args))

(defn doc
  "Adds a description string to a type

   {:postalCode (doc :String
                  'Should not contain spaces'}"
  [t docstring]
  (assoc (as-type-map t) :description docstring))

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
;; Macros

(m/defn ^:private parse-args [[name & args]]
  (let [[doc args] (if (string? (first args))
                     [(first args) (rest args)]
                     [nil args])
        [options args] (if (map? (first args))
                         [(first args) (rest args)]
                         [nil args])
        [arglist body-1] (if (vector? (first args))
                           [(first args) (second args)]
                           [nil (first args)])]
    [name doc options arglist body-1]))

(m/defn ^:private field [operation [name & args]]
  (let [[name doc options arglist body-1] (parse-args (cons name args))
        type-map (-> {:name (str name)
                      :description doc
                      :args (or (some-> (first arglist)
                                        (u/update-keys keyword))
                                {})
                      :type-key body-1}
                     (assoc :operation operation))]
    `(merge (new Root)
            {:schema/type-map ~type-map
             :root/variables {}
             :root/options {}}
            ~options)))

(m/defmacro query [name & [doc options arglist return-type]]
  (field :query [name doc options arglist return-type]))

(m/defmacro mutation [name & [doc options arglist return-type]]
  (field :mutation [name doc options arglist return-type]))

(m/defmacro subscription [name & [doc options arglist return-type]]
  (field :subscription [name doc options arglist return-type]))

(m/defmacro defquery [name & [doc options arglist return-type]]
  `(def ~name ~(field :query [name doc options arglist return-type])))

(m/defmacro defmutation [name & [doc options arglist return-type]]
  `(def ~name ~(field :mutation [name doc options arglist return-type])))

(m/defmacro defsubscription [name & [doc options arglist return-type]]
  `(def ~name ~(field :subscription [name doc options arglist return-type])))

(s/def ::name string?)
(s/def ::type-key (s/and keyword?
                         registered-type?))
(s/def ::type-key+ (s/or :type-key keyword?
                         :type-key-plural (s/and vector?
                                                 (s/coll-of keyword? :count 1))))
(s/def ::type-args vector?)
(s/def ::operation keyword?)
(s/def ::description string?)
(s/def ::implementation-of keyword?)
(s/def ::fields-map (s/map-of keyword? ::type-key+))
(s/def ::type-keys (s/coll-of ::type-key))
(s/def ::enum-values (s/coll-of (s/or :string string?
                                      :keyword keyword?)))

(s/def ::type-map
  (s/keys :req-un [::name
                   ::type-key
                   ::data
                   ::implementation-of
                   ::type-args]
          :opt-un [::description
                   ::operation]))

(defn type-args [data-type]
  (s/cat :type-key keyword?
         :doc (s/? ::description)
         :data data-type))

(s/fdef object
        :args (type-args ::fields-map)
        :ret ::type-key)
(s/fdef input
        :args (type-args ::fields-map)
        :ret ::type-key)
(s/fdef interface
        :args (type-args ::fields-map)
        :ret ::type-key)
(s/fdef union
        :args (type-args ::type-keys)
        :ret ::type-key)
(s/fdef enum
        :args (type-args ::enum-values)
        :ret ::type-key)

(s/def :graphql/cache
  #?(:cljs #(satisfies? IDeref %)
     :clj  #(instance? clojure.lang.Atom %)))

(s/def :graphql/xvec
  (s/and vector?
         (s/cat :tag keyword?
                :props (s/? map?)
                :xkeys (s/* :graphql/xkey))))

(s/def :graphql/xkey
  (s/or :simple-key keyword?
        :xvec-key :graphql/xvec
        :splice (s/and seq?
                       (s/coll-of :graphql/xkey))))



(s/def :graphql/xkeys (s/coll-of :graphql/xkey))

(s/def :root/xkeys :graphql/xkeys)

(comment
 (s/valid? :graphql/xkey (list :a [:b {}]))
 (s/valid? :graphql/xkeys (list
                           :a
                           [:b {} :c]
                           (map keyword ["d" "e"])
                           )))

#?(:cljs
   (when goog/DEBUG
     (set! s/*explain-out* expound/printer)
     (st/instrument)))