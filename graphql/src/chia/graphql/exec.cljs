(ns chia.graphql.exec
  (:require [chia.graphql.schema :as schema]
            [chia.util.js-interop :as j]
            [chia.x-vec :as x]
            [chia.graphql.normalize :as n]
            [goog.Promise :as promise]
            [kitchen-async.promise :as p]

            [clojure.spec.alpha :as s]
            [cljs.pprint :as pp]
            [chia.graphql :as g]))

(defn form-key [form]
  (cond-> form (vector? form) (first)))

(defn get-field [parent field-name]
  (if (satisfies? ILookup parent)
    (or (get parent field-name)
        (when (string? field-name)
          (get parent (keyword field-name))))
    (j/get parent field-name)))

(defn default-resolver [[parent variables context {:keys      [form]
                                                   parent-key :schema/type-key}]]
  (let [field-key (form-key form)
        ]
    )
  (prn :f form :v variables)
  (assert (empty? variables))
  (get-field parent (form-key form)))

(defn resolve-return-type [registry {:as            entry
                                     entry-type-key :schema/type-key} value]
  ;; cases
  ;; - Union - entry-resolver or is-in-set
  ;; - Interface - entry-resolver or is-in-set
  ;; - Object-y - value is a map and does not contain a conflicting type-key
  ;; - Scalar - value is expected type

  ;; for object types, add :type-key / :__typename to `value`.

  #_(cond (= entry-type value-type) true
          (schema/abstract-types entry-type) (and value-type
                                                  (case entry-type
                                                     )
                                                  ;; Union - check if in set
                                                  ;; Interface - check if in set

                                                  )
          (nil? value-type)
          (when (schema/abstract-types entry-type))
          (and (nil? value-type)
               (not (schema/abstract-types entry-type)))))

(defn selectable? [registry type-key]
  )

(defn parse-form [_variables form]
  (if (keyword? form)
    [form nil nil]
    (let [tag (form 0)
          [variables children] (x/parse-vec form)]
      [tag
       (n/resolve-variables variables _variables)
       (when children
         (n/parse-keys (name tag) _variables children))])))

(declare exec-field)

(defn parse-result [registry parent context {:as   info
                                             :keys [_registry
                                                    _wrap-resolver
                                                    _default-resolver
                                                    _variables

                                                    field/key
                                                    field/alias
                                                    field/variables
                                                    field/children
                                                    field/return-key
                                                    field/entry]}]

  ;; 1. materialize abstract types

  ;; 2.

  ;; what we have to do here... whether singular or plural:
  ;; handle/resolve type info, for ambiguous types
  ;; get child keys, for object types

  #_(let [[type-key root-result] (schema/resolve-runtime-type parent return-key)]
      (if (schema/concrete?))
      )
  #_(let [[field-key variables xkeys] (parse-form form)]

      (p/let [child-results (when (seq xkeys)
                              (->> (for [child-k xkeys]
                                     (exec-field registry parent context (assoc info :form child-k)))
                                   (to-array)
                                   (promise/all)))]
        (->> (interleave xkeys child-results)
             (partition 2)
             (reduce (fn [out [k result]]
                       (assoc out (cond-> k
                                          (vector? k) (first)) result)) {})))))

(defn child-info [info [req-key lookup-key children]]
  (let [[field-key variables] (if (vector? lookup-key)
                                lookup-key
                                [lookup-key nil])
        entry (schema/entry (:_registry info) field-key)]
    (-> info
        (merge
         #:field{:key        field-key
                 :alias      req-key
                 :variables  variables
                 :children   (some->> children
                                      (n/parse-keys (name field-key) (:_variables info)))
                 :return-key (:schema/type-key entry)
                 :entry      entry}))))

(defn exec-field [parent context {:as   info
                                  :keys [_registry
                                         _wrap-resolver
                                         _default-resolver
                                         _variables

                                         field/key
                                         field/alias
                                         field/variables
                                         field/children
                                         field/return-key
                                         field/entry]}]
  (pp/pprint {:parent parent
              :field  {:key   key
                       :alias alias
                       :vars  variables
                       :chil  children
                       :ret   return-key
                       :en    entry}})
  (let [resolver (or (some-> (:resolve entry)
                             (_wrap-resolver))
                     _default-resolver)]
    (p/let [result (resolver [parent variables context info])
            result (if (schema/is-list? return-key)
                     (p/all (mapv #(parse-result _registry parent context info) result))
                     (parse-result _registry result context info))]
      result)))

(defn exec [{:keys      [form
                         context
                         registry
                         wrap-resolver
                         parent
                         schema/type-key]
             _variables :variables
             :or        {registry         @schema/*schema-ref*
                         wrap-resolver    identity
                         default-resolver default-resolver
                         parent           {}}}]
  (let [parsed-keys (n/parse-keys (name type-key) _variables [form])
        _ (prn :vars _variables)
        _ (prn :parsed-keys parsed-keys)
        _ (assert (= 2 (count parsed-keys)))
        info (child-info {:_wrap-resolver    wrap-resolver
                          :_default-resolver default-resolver
                          :_variables        _variables
                          :_registry         registry}
                         (first parsed-keys))]
    (exec-field parent context info))
  #_(let [resolver (some-> (schema/entry root)
                           :resolve
                           (wrap-resolver))]
      (assert resolver "No resolver present")
      (exec* registry [{} variables context {:wrap-resolver    wrap-resolver
                                             :default-resolver default-resolver
                                             :field-k          (:schema/type-key root)}])

      ))

