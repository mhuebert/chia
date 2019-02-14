(ns chia.graphql.specs
  (:require [chia.graphql.schema :as schema]
            #?(:cljs [chia.graphql.types :as types])
            [chia.graphql.normalize :as n]
            [chia.graphql.request :as request]
            [chia.util :as u]
            [chia.graphql.root :as root]
            [applied-science.js-interop :as j]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

;; schema

(s/def :schema/registry
  (s/map-of keyword? (s/or :type-map :schema/type-map
                           :root #(instance? schema/Root %))))

(s/fdef schema/make-schema
        :args (s/cat :fields (s/keys
                              :opt-un [:schema/registry])))

(s/def :schema/terminal-type #?(:cljs types/type?
                                :clj  (constantly true)))

(s/fdef schema/type-map-terminal
        :ret :schema/terminal-type)

(s/fdef schema/type-key-terminal
        :ret :schema/terminal-type)


(s/def ::schema/name string?)
(s/def ::schema/type-key keyword? #_(s/and keyword?
                                schema/registered-type?))
(s/def ::schema/type-key+ (s/or :schema/type-key keyword? #_::schema/type-key
                                :type-key-plural (s/and vector?
                                                        (s/coll-of keyword? :count 1))))
(s/def ::schema/args map?)
(s/def ::schema/operation keyword?)
(s/def ::schema/description string?)
(s/def ::schema/of-interface keyword?)
(s/def ::schema/fields-map (s/map-of keyword? (s/or
                                               :schema/type-key ::schema/type-key+
                                               :type-map ::schema/type-map
                                               :child-fields (s/and map?
                                                                         ::schema/fields-map)
                                               )))
(s/def ::schema/type-keys (s/coll-of ::schema/type-key))
(s/def ::schema/enum-values (s/coll-of (s/or :string string?
                                             :keyword keyword?)))

(s/def ::schema/type-map
  (s/keys :req-un [::schema/name
                   ::schema/type-key]
          :opt-un [::schema/description
                   ::schema/of-interface
                   ::schema/args
                   ::schema/data]))

(defn type-args [data-type]
  (s/cat :schema/type-key keyword?
         :doc (s/? ::schema/description)
         :data data-type))

(s/fdef schema/object
        :args (type-args ::schema/fields-map)
        :ret ::schema/type-key)
(s/fdef schema/input
        :args (type-args ::schema/fields-map)
        :ret ::schema/type-key)
(s/fdef schema/interface
        :args (type-args ::schema/fields-map)
        :ret ::schema/type-key)
(s/fdef schema/union
        :args (type-args ::schema/type-keys)
        :ret ::schema/type-key)
(s/fdef schema/enum
        :args (type-args ::schema/enum-values)
        :ret ::schema/type-key)

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


;; normalize

(s/fdef n/parse-keys
        :args (s/cat :name (s/nilable string?)
                     :variables (s/nilable map?)
                     :xkeys :graphql/xkeys))

(s/fdef n/read-keys
        :args (s/cat :cache :graphql/cache
                     :id any?
                     :variables (s/? map?)
                     :child-keys (s/+ :graphql/xkey)))

(s/fdef n/normalize-incoming
        :args (s/cat :cache :graphql/cache
                     :variables (s/nilable map?)
                     :xkeys :graphql/xkeys
                     :data (s/nilable object?)))

(s/fdef n/form-datoms
        :args (s/cat :cache :graphql/cache
                     :form :graphql/xvec
                     :variables map?
                     :payload object?))

(s/fdef n/read-fragment
        :args (s/cat :cache :graphql/cache
                     :form (s/and vector?
                                  n/fragment?)
                     :entity-id any?
                     :variables (s/? map?)))

;; request

(s/def ::request/token (s/or :string string?
                             :fn fn?
                             :promise u/promise?))

(s/def ::request/url string?)

(s/def ::request/api-options (s/keys :req-un [::request/token
                                              ::request/url]))

(s/fdef request/post!
        :args (s/cat :options ::request/api-options
                     :form :graphql/xvec
                     :variables (s/nilable map?)))

;; root

(s/fdef root/get-api-options
        :ret (s/cat :options ::request/api-options))

(s/fdef root/set-api-options!
        :args (s/cat :options ::request/api-options))

(s/fdef root/get-operation
        :args (s/cat :root :graphql/root))

(s/def :graphql/root
  (s/and #(instance? schema/Root %)
         (s/keys
          :req [:root/xkeys
                :root/variables])))

(s/def :graphql/root-id
  (s/map-of string? number?
            :count 1))

(s/fdef root/root-id
        :args (s/cat :root :graphql/root)
        :ret :graphql/root-id)

(s/fdef root/merge-root-meta!
        :args (s/cat :root :graphql/root
                     :attrs (s/nilable map?)))

(s/fdef root/root-entry
        :args (s/cat :root :graphql/root))

(s/fdef root/on-lifecycle!
        :args (s/cat :root :graphql/root
                     :lifecycle-key #{:on-unmount}
                     :listener-key any?
                     :fn fn?))

(s/fdef root/mount-view!
        :args (s/cat :root :graphql/root
                     :options (s/nilable map?)
                     :view some?))

(s/def :root/on-unmount fn?)

(s/fdef root/listen
        :args (s/cat :root :graphql/root
                     :options (s/and map?
                                     (s/keys :opt-un [:root/on-unmount]))
                     :view any?))

(s/fdef root/root-datoms
        :args (s/cat :cache :graphql/cache
                     :root :graphql/root
                     :payload (s/and object?
                                     #(or (j/contains? % :data)
                                          (j/contains? % :errors)))))
(s/fdef root/read-root
        :args (s/or :arity-1 (s/cat :root :graphql/root)
                    :arity-2 (s/cat :cache :graphql/cache
                                    :root :graphql/root)))

(s/fdef root/write-root-response!
        :args (s/cat :root :graphql/root
                     :payload (s/nilable object?)))

;(st/instrument)