(ns chia.graphql.normalize
  (:require [chia.graphql :as g]
            [chia.x-vec :as x]
            [chia.triple-db.core :as d]
            [chia.util :as u]
            [chia.util.js-interop :as j]
            [clojure.string :as str]))

(defn req-id [{:as req
               :keys [query variables]}]
  (assert query "Must provide :query")
  {query (or variables {})})

(def ^:dynamic *map->id* :id)
(def ^:dynamic *fragment-matches?* (constantly true))

(defn get-data* [o k]
  (if (satisfies? ILookup o)
    (-lookup o k nil)
    (j/get o k nil)))

#_(defn create! [& [{:as options
                     :keys [fragment-matches?
                            map->id]
                     :or {map->id :id
                          fragment-matches? (constantly true)}}]])

(def default-cache (d/create))

(deftype EntityRef [cache id]
  #_#_IPrintWithWriter
      (-pr-writer [this writer opts]
                  (-write writer (str "#Entity<" id ">")))
  ILookup
  (-lookup [o k]
    (d/get cache id k))
  (-lookup [o k not-found]
    (d/get cache id k not-found))
  IEquiv
  (-equiv [o other]
    (and (= (type o) (type other))
         (= id (.-id other))))
  IDeref
  (-deref [_]
    (d/entity cache id)))

(defn ref? [x] (= (type x) EntityRef))

(defn children [query-form]
  (-> (cond-> query-form
              (satisfies? g/IGraphQL query-form)
              (-> :form (deref)))
      (x/parse-vec)
      second))

(defn fragment? [query-form]
  (or (and (vector? query-form) (= :... (first query-form)))
      (and (satisfies? g/IGraphQL query-form)
           (= "fragment" (get query-form :operation)))))

(defn query? [query-form]
  (and (satisfies? g/IGraphQL query-form)
       (= "query" (get query-form :operation))))

(defn fragment-typename [query-form]
  (let [opts (second (cond (vector? query-form) query-form
                           (satisfies? g/IGraphQL query-form) @(:form query-form)))]
    (some-> (or (get opts :gql/on)
                (get opts :on))
            (name))))

(defn strip-$ [s]
  (cond-> s
          (str/starts-with? s "$") (subs 1)))

(defn resolve-variables [variables props]
  (reduce-kv (fn [m k v]
               (if (and (keyword? v)
                        (str/starts-with? (name v) "$"))
                 (assoc m k (get variables (keyword (strip-$ (name v)))))
                 m)) props props))

(defn parse-query-keys*
  "Returns a list of parsed keys, of the form: [req-key, cache-key, child-keys]"
  [__typename variables query-form]
  (let [query-keys (cond-> query-form
                           (satisfies? g/IGraphQL query-form) (children))]
    (->> query-keys
         (reduce (fn [out k]
                   (cond (keyword? k) (conj out [k k nil])
                         (fragment? k) (cond-> out
                                               (*fragment-matches?* __typename (fragment-typename k))
                                               (into (parse-query-keys* __typename variables (children k))))

                         (vector? k) (let [[{:as props
                                             :keys [gql/alias-of]} child-keys] (x/parse-vec k)
                                           props (-> props
                                                     (dissoc :gql/alias-of)
                                                     (u/guard seq))
                                           req-key (first k)
                                           cache-key (let [simple-k (or alias-of req-key)]
                                                       (if props [simple-k (resolve-variables variables props)]
                                                                 simple-k))]
                                       (conj out [req-key cache-key (seq child-keys)]))
                         (seq? query-keys) (into out (mapv #(parse-query-keys* __typename variables %) query-keys))
                         (nil? k) out
                         :else (throw (ex-info "Invalid key" {:key k}))))
                 []))))

(def parse-query-keys (memoize
                       (comp #(when (seq %)
                                (conj % [:__typename :__typename nil]))
                             parse-query-keys*)))

(defn read-query* [cache variables data query-form]
  (let [__typename (:__typename data)
        parsed-keys (parse-query-keys (:__typename data) variables query-form)]
    (->> parsed-keys
         (reduce (fn [m [req-key
                         cache-key
                         child-keys]]
                   (let [v (get data cache-key)]
                     (assoc m req-key
                              (if child-keys
                                (if (vector? v)
                                  (mapv #(read-query* cache variables % child-keys) v)
                                  (read-query* cache variables v child-keys))
                                v)))) {}))))

(defn read-query [cache {:as req
                         :keys [id
                                query
                                variables]}]
  (let [query-id (req-id req)
        query-cache (d/entity cache query-id)
        ;; if id is provided, or if we can derive id from variables,
        ;; then read the normalized entity
        root-data (if id
                    (EntityRef. cache id)
                    (:async/value query-cache))
        value (read-query* cache variables root-data query)
        flat-value (some-> (dissoc value :__typename)
                           (u/guard #(= (count %) 1))
                           (first)
                           (second))
        value (cond-> (or flat-value value)
                      (= 1 (count value)) (-> (first)
                                              (second)))]
    (cond-> {:async/value value}
            (some? query-cache)
            (merge (select-keys root-data [:async/loading?
                                           :async/error])))))

(defn read-keys [cache {:as req
                        :keys [id
                               query
                               variables]} & child-keys]
  (read-query* cache variables
               (EntityRef. cache (or id (req-id req)))
               (or (seq child-keys)
                   (when query (children query)))))

(def ^:dynamic *datoms* nil)

(defn normalize-response* [cache variables query-form data]
  (when data
    (let [__typename (j/get data :__typename)
          parsed-keys (parse-query-keys __typename variables query-form)
          value (->> parsed-keys
                     (reduce
                      (fn [m [req-key
                              cache-key
                              child-keys]]
                        (let [value (get-data* data req-key)
                              normalized-value (if child-keys
                                                 (if (array? value)
                                                   (mapv #(normalize-response* cache variables child-keys %) value)
                                                   (normalize-response* cache variables child-keys value))
                                                 (js->clj value :keywordize-keys true))]
                          (assoc m cache-key normalized-value))) {}))
          id (*map->id* value)
          pointer-or-value (if id (EntityRef. cache id)
                                  value)]
      (when id
        (vswap! *datoms* conj (assoc value :db/id id)))
      pointer-or-value)))

(defn normalize-response-data [cache req data]
  (binding [*datoms* (volatile! [])]
    (let [root (normalize-response* cache (:variables req) (:query req) data)]
      {:datoms @*datoms*
       :root root})))

(defn format-errors [errors]
  (some-> errors
          (first)
          (js->clj :keywordize-keys true)))

(defn response-datoms [cache req response]
  (let [id (req-id req)
        {:keys [data
                errors]} (j/lookup response)
        {:keys [datoms
                root]} (normalize-response-data cache req data)]
    (conj datoms
          {:db/id id
           :async/value root
           :async/error errors
           :async/loading? false})))

(defn cache-response! [cache req response]
  (d/transact! cache (response-datoms cache req response)))
