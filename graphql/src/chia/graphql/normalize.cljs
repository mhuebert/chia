(ns chia.graphql.normalize
  (:require [chia.graphql :as g]
            [chia.x-vec :as x]
            [chia.triple-db.core :as d]
            [chia.util :as u]
            [chia.util.js-interop :as j]
            [clojure.string :as str]
            [chia.graphql.schema :as schema]
            [cljs.pprint :as pp]))

(defonce default-cache (d/create))

(def ^:dynamic *map->id* :id)

(def ^:dynamic *fragment-matches?*
  (fn [data-type fragment-type]
    (or (nil? fragment-type)
        (= data-type fragment-type)
        (some-> (schema/implementors-simple @schema/*schema-ref*)
                (get fragment-type)
                (contains? data-type)))))

(defn get-data* [o k]
  (if (satisfies? ILookup o)
    (-lookup o k nil)
    (j/get o k nil)))


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
    (d/entity cache id))
  Object
  (toString [this]
    (str "EntityRef[" id "]")))

(defn ref? [x] (= (type x) EntityRef))

(defn fragment? [form]
  (or (= :... (form 0))
      (= :fragment (g/get-operation form))))

(defn fragment-typename [form]
  (let [opts (g/options form)]
    (some-> (get opts :on)
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

(defn parse-keys
  "Returns a list of parsed keys, of the form: [alias, lookup-key, child-keys].

  - only includes children where fragment matches
  "
  [data__typename variables child-keys]
  (assert (sequential? child-keys))

  (->> child-keys
       (reduce (fn [out k]
                 (cond (keyword? k) (conj out [k k nil])
                       (vector? k) (if (fragment? k)
                                     (cond-> out
                                             (*fragment-matches?* data__typename (fragment-typename k))
                                             (into (parse-keys data__typename variables (g/children k))))
                                     (let [[{:as   props
                                             :keys [graphql/alias-of]} child-keys] (x/parse-vec k)
                                           props (-> props
                                                     (dissoc :graphql/alias-of)
                                                     (u/guard seq))
                                           alias (first k)
                                           lookup-key (let [simple-k (or alias-of alias)]
                                                        (if props [simple-k (resolve-variables variables props)]
                                                                  simple-k))]
                                       (conj out [alias lookup-key (seq child-keys)])))

                       (seq? child-keys) (into out (mapv #(parse-keys data__typename variables %) child-keys))
                       (nil? k) out
                       :else (throw (ex-info "Invalid key" {:key  k
                                                            :form child-keys}))))
               #{[:__typename :__typename nil]})))


#_(def parse-keys (memoize
                   (comp #(when (seq %)
                            (conj % [:__typename :__typename nil]))
                         parse-keys*)))



(defn read-keys* [cache variables data child-keys]
  (when data
    (let [__typename (:__typename data)
          parsed-keys (parse-keys __typename variables child-keys)]
      (->> parsed-keys
           (reduce (fn [m [alias
                           lookup-key
                           child-keys]]
                     (let [v (get data lookup-key)]
                       (assoc m alias
                                (when (some? v)
                                  (if child-keys
                                    (if (vector? v)
                                      (mapv #(read-keys* cache variables % child-keys) v)
                                      (read-keys* cache variables v child-keys))
                                    v))))) {})))))

(defn read-keys [cache id variables & child-keys]
  (let [[variables child-keys] (if (map? variables)
                                 [variables child-keys]
                                 [nil (cons variables child-keys)])]
    (read-keys* cache variables
                (EntityRef. cache id)
                child-keys)))

(defn read-fragment
  ([cache form entity-id]
   (read-fragment cache form entity-id {}))
  ([cache form entity-id variables]
   (assert (fragment? form))
   (read-keys* cache
               variables
               (d/entity cache entity-id)
               (g/children form))))

(def ^:dynamic *datoms* nil)

(defn merge-maps [x y]
  (if (map? x)
    (merge x y)
    y))

(comment
 (assert (-> (merge-with merge-maps
                         {:name {:first "A"}}
                         {:name nil})
             :name
             nil?)))

(defn normalize-incoming [cache variables child-keys data]
  (when data
    (let [__typename (get-data* data :__typename)
          parsed-keys (parse-keys __typename variables child-keys)
          value (->> parsed-keys
                     (reduce
                      (fn [m [alias
                              lookup-key
                              child-keys]]
                        (let [value (get-data* data alias)
                              normalized-value (if child-keys
                                                 (if (array? value)
                                                   (mapv #(normalize-incoming cache variables child-keys %) value)
                                                   (normalize-incoming cache variables child-keys value))
                                                 (js->clj value :keywordize-keys true))]
                          (assoc m lookup-key normalized-value))) {}))]
      (if-let [id (*map->id* value)]
        (do (vreset! *datoms*
                     (reduce (fn [out [attr val]]
                               (conj out
                                     (cond (map? val)
                                           [:db/update-attr id attr #(merge-with merge-maps % val)]
                                           (nil? val)
                                           [:db/retract-attr id attr]
                                           :else
                                           [:db/add id attr val]))) @*datoms* (seq value)))
            (EntityRef. cache id))
        value))))

(defn format-errors [errors]
  (some-> errors
          (first)
          (js->clj :keywordize-keys true)))

(defn form-datoms [cache form variables data]
  (binding [*datoms* (volatile! [])]
    (let [root (normalize-incoming cache variables (g/children form) data)
          datoms (cond-> @*datoms*
                         (not (fragment? form)) (into
                                                 (for [[root-k root-v] root]
                                                   [:db/update-attr :cache/root root-k #(u/merge-maps % root-v)])))]
      datoms)))
