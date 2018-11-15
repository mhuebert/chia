(ns chia.graphql.schema
  (:require [chia.util :as u]
            [chia.util.macros :as m])
  #?(:cljs (:require-macros [chia.graphql.schema])))

(m/defn ^:private parse-args [[name & args]]
  (let [[doc args] (if (string? (first args))
                     [(first args) (rest args)]
                     [nil args])
        [params type-key] (if (vector? (first args))
                            [(ffirst args) (second args)]
                            [nil (first args)])]
    {:name (str name)
     :description doc
     :args (some-> params
                   (u/update-keys keyword))
     :type-key type-key}))

(m/defn ^:private def-field [[name & args]]
  (let [type-map (parse-args (cons name args))]
    `(def ~name ~type-map)))

(m/defmacro query [name & [doc params return-type]]
  (def-field [name doc params return-type]))

(m/defmacro mutation [name & [doc params return-type]]
  (def-field [name doc params return-type]))

(m/defmacro subscription [name & [doc params return-type]]
  (def-field [name doc params return-type]))

(defonce ^:dynamic *registry* (atom {:gql/implementors {}}))

(defn- typespace [k]
  (keyword (namespace k)
           (re-find #"^[^.]+" (name k))))

(defn- typekey [k]
  (some-> (re-find #"[^.]+$" (name k))
          (keyword)))

(def ^:private typename (comp name typekey))

(defn- keyword-append [k s]
  (keyword (namespace k)
           (str (name k) s)))

(defn- typekey-append [typespace k]
  (keyword-append typespace (str "." (name k))))

(defn- inherit-typespace
  "Join unqualified child key to a parent typespace"
  [typespace child-k]
  (typekey-append typespace (typekey child-k)))

(defn- external-keys
  "Keys which cannot inherit from parent typespace (g. interfaces)"
  [parent-typespace ks]
  (-> (into #{} (comp (filter namespace)
                      (map typespace)) ks)
      (disj parent-typespace)))

(defn- as-type-map [t]
  (cond (map? t) t
        (or (keyword? t)
            (vector? t)) {:type-key t}
        :else (throw (ex-info (str "Invalid type: " t) {:t t}))))


(comment
 (assert (= (external-keys ::Thread [::Entity.x
                                     ::Thread.y
                                     :z])
            '(::Entity))))

(defn field-entries
  ([object-typespace data]
   (field-entries {} object-typespace data))
  ([m object-typespace data]
   (->> data
        (reduce-kv
         (fn [entries k v]
           (assoc entries (inherit-typespace object-typespace k) (as-type-map v)))
         m))))

(defn- fields-entry [parent-key]
  (fn [m {:keys [type-key
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
      (-> m
          (merge fields)
          (update :gql/implementors (fn [m]
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
      (update-in [:gql/implementors type-key] (fnil into #{}) types)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Public API for defining types

(defn object [key & [doc fields]]
  (swap! *registry* register-type
         (fields-entry :Object)
         (args->map key doc fields)))

(defn input [key & [doc fields]]
  (swap! *registry* register-type
         (fields-entry :InputObject)
         (args->map key doc fields)))

(defn interface [key & [doc fields]]
  (swap! *registry* register-type
         (fields-entry :Interface)
         (args->map key doc fields)))

(defn union [key & [doc type-keys]]
  (swap! *registry* register-type
         union-entry
         (args->map key doc type-keys)))

(defn enum [key & [doc values]]
  (swap! *registry* register-type
         enum-entry
         (args->map key doc values)))

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



(comment
 (assert (= (typespace ::A.b) ::A))
 (assert (= (typespace ::A) ::A))
 (assert (= (typekey ::A.b) :b))
 (assert (= (keyword-append ::A.b "!")
            ::A.b!))
 (assert (= (typekey-append ::A.b "c")
            ::A.b.c)))