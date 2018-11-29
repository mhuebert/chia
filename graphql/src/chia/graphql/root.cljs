(ns chia.graphql.root
  (:require [clojure.spec.alpha :as s]
            [chia.graphql :as g]
            [chia.graphql.schema :as schema]
            [chia.util :as u]
            [chia.triple-db.core :as d]
            [chia.util.js-interop :as j]
            [chia.graphql.normalize :as n]
            [chia.graphql.request :as request]
            [kitchen-async.promise :as p]
            [chia.view :as v])
  (:require-macros [chia.graphql.root :as root]))

(defonce cache (d/create))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API Options

(defonce api-options (atom {}))

(defn get-api-options []
  @api-options)

(s/fdef get-api-options
        :ret (s/cat :options ::request/api-options))

(defn set-api-options! [opts]
  (reset! api-options opts))

(s/fdef set-api-options!
        :args (s/cat :options ::request/api-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Root type

(def get-name (comp :name :schema/type-map))
(def get-operation (comp keyword :operation :schema/type-map))

(s/fdef get-operation
        :args (s/cat :root :graphql/root))

(defn root-id [root]
  {(get-name root) (hash (case (get-operation root)
                           :mutation (select-keys root [:root/xkeys
                                                        :root/view])
                           :query (select-keys root [:root/xkeys
                                                     :root/variables])))})

(s/def :graphql/root
  (s/and #(instance? schema/Root %)
         (s/keys
          :req [:root/xkeys
                :root/variables])))

(s/def :graphql/root-id
  (s/map-of string? number?
            :count 1))

(s/fdef root-id
        :args (s/cat :root :graphql/root)
        :ret :graphql/root-id)

(defn with-xkeys [root & ks]
  (assoc root :root/xkeys ks))

(defn with-options [root variables]
  (assoc root :root/options variables))

(defn merge-root-meta! [root attrs]
  (d/transact! cache [(-> attrs
                          (assoc :db/id (root-id root)))]))

(s/fdef merge-root-meta!
        :args (s/cat :root :graphql/root
                     :attrs (s/nilable map?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keep track of view<>root relationships

(defonce ^:private root-listeners (volatile! {}))

(defn- root-entry [root]
  (get @root-listeners (root-id root)))

(s/fdef root-entry
        :args (s/cat :root :graphql/root))

(defn- maybe-unmount-root [root]
  (let [{:as entry
         :keys [views
                lifecycle]} (root-entry root)]
    (when (empty? views)
      (doseq [on-unmount (-> lifecycle
                             :on-unmount
                             (vals))]
        (assert (fn? on-unmount))
        (on-unmount root))
      (d/transact! cache [[:db/retract-entity (root-id root)]]))))

(defn- unmount-view! [root view]
  (vswap! root-listeners update-in [(root-id root) :views] disj view)

  ;; wait before unmounting to avoid rapid unmount/mount cycles during page transitions
  (js/setTimeout #(maybe-unmount-root root) 500))

(defn on-lifecycle!
  "Register a callback to be evaluated when `req` is no longer listened to by any views."
  [root lifecycle-k listener-k f]
  (vswap! root-listeners assoc-in [(root-id root)
                                   :lifecycle
                                   lifecycle-k
                                   listener-k] f))

(s/fdef on-lifecycle!
        :args (s/cat :root :graphql/root
                     :lifecycle-key #{:on-unmount}
                     :listener-key any?
                     :fn fn?))

(defn- mount-view! [root options view]
  (let [mount-root? (-> (root-entry root)
                        :views
                        (empty?))]
    (vswap! root-listeners update-in [(root-id root) :views] (fnil conj #{}) view)
    (v/on-unmount! view root #(unmount-view! root view))
    (when mount-root?
      (doseq [f [(:on-unmount options)
                 (:on-unmount (:root/options root))]]
        (when f
          (on-lifecycle! root :on-unmount view f)))
      (doseq [f [(:on-mount options)
                 (:on-mount (:root/options root))]]
        (when f
          (f root))))))

(s/fdef mount-view!
        :args (s/cat :root :graphql/root
                     :options (s/nilable map?)
                     :view some?))

(defn listen [root options view]
  (when-not (some-> (root-entry root)
                    :views
                    (contains? view))
    (mount-view! root options view)))

(s/def ::on-unmount fn?)

(s/fdef listen
        :args (s/cat :root :graphql/root
                     :options (s/and map?
                                     (s/keys :opt-un [::on-unmount]))
                     :view any?))

(defn form [root]
  (let [{:keys [root/xkeys
                schema/type-map]} root]
    (let [{field-name :name
           :keys [args
                  operation]} type-map
          operation-params (u/update-keys args
                                          #(keyword (u/ensure-prefix (name %) "$")))
          field-params (->> (keys operation-params)
                            (reduce (fn [m k]
                                      (assoc m (keyword (subs (name k) 1)) k)) {}))]
      [(keyword operation field-name) operation-params
       `[~(keyword field-name) ~field-params ~@xkeys]])))

(defn root-datoms [cache root payload]
  (let [{:keys [data
                errors]} (j/lookup payload)
        form (form root)
        variables (:root/variables root {})
        cache-keys (->> (g/children form)
                        (n/parse-keys nil variables)
                        (mapv second))
        datoms (when data
                 (n/form-datoms cache form variables data))]
    (conj datoms {:db/id (root-id root)
                  :async/error (js->clj errors :keywordize-keys true)
                  :async/loading? false
                  :graphql.root/cache-keys cache-keys
                  :graphql.root/variables variables})))

(s/fdef root-datoms
        :args (s/cat :cache :graphql/cache
                     :root :graphql/root
                     :payload (s/and object?
                                     #(or (j/contains? % :data)
                                          (j/contains? % :errors)))))

(defn add-to-cache! [cache root-or-form payload]
  (->> (if (vector? root-or-form)
         (n/form-datoms cache root-or-form {} (j/get payload :data))
         (root-datoms cache root-or-form payload))
       (d/transact! cache)))

(defn read-root
  ([root]
   (read-root cache root))
  ([cache root]
   (let [entity-id (root-id root)]
     (when-let [{:as root-entry
                 :keys [graphql.root/variables
                        graphql.root/cache-keys]} (d/entity cache entity-id)]
       (assert root "root-entry must contain root")
       (let [form (form root)
             value (n/read-keys* cache variables
                                 (d/select-keys cache :cache/root cache-keys)
                                 (g/children form))]
         (merge value
                (select-keys root-entry [:async/loading?
                                         :async/error])))))))

(s/fdef read-root
        :args (s/or :arity-1 (s/cat :root :graphql/root)
                    :arity-2 (s/cat :cache :graphql/cache
                                    :root :graphql/root)))


(def read-fragment (partial n/read-fragment cache))

(defn write-root-response! [root payload]
  (add-to-cache! cache root payload)
  (read-root cache root))

(s/fdef write-root-response!
        :args (s/cat :root :graphql/root
                     :payload (s/nilable object?)))

(defn post! [root]
  (merge-root-meta! root
                    {:async/loading? true})
  (-> (request/post! (get-api-options)
                     (form root)
                     (:root/variables root))
      (p/then (fn [response]
                (let [{:keys [async/error]
                       :as result} (write-root-response! root response)]
                  result)))
      (p/catch* (fn [error]
                  (js/console.error error)
                  (merge-root-meta! root
                                    {:async/error {:message "Error sending GraphQL operation"
                                                   :error error}})))))



(defn group-options [m]
  (reduce-kv (fn [[variables options] k v]
               (case (namespace k)
                 "graphql"
                 [variables (assoc options k v)]
                 [(assoc variables k v) options])) [{} {}] m))

(defmulti invoke get-operation)

(defmethod invoke :mutation
  [root]

  (some->> v/*current-view*
           (listen root {}))

  (assoc (read-root root)
    :mutate! (fn [& [variables-options]]
               (let [[variables-2 options-2] (group-options variables-options)]
                 (post! (-> root
                            (update :root/variables merge variables-2)
                            (update :root/options merge options-2)))))))

(defmethod invoke :query
  [root]

  (some->> v/*current-view*
           (listen root {:on-mount post!}))

  ;; handle cache policies - :network-only, :cache-only, :cache-and-network, :cache-or-network
  (read-root root))

(extend-type schema/Root
  IFn
  (-invoke
    ([this variables]
     (root/invoke* this
                   variables))
    ([this variables k1]
     (root/invoke* this
                   variables
                   [k1]))
    ([this variables k1 k2]
     (root/invoke* this
                   variables
                   [k1 k2]))
    ([this variables k1 k2 k3]
     (root/invoke* this
                   variables
                   [k1 k2 k3]))
    ([this variables k1 k2 k3 k4]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4]))
    ([this variables k1 k2 k3 k4 k5]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4 k5]))
    ([this variables k1 k2 k3 k4 k5 k6]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4 k5 k6]))
    ([this variables k1 k2 k3 k4 k5 k6 k7]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7 k8]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8 k9]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7 k8 k9]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8 k9 k10]
     (root/invoke* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7 k8 k9 k10]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 rest]
     (assert (sequential? rest) "Calling a top-level GraphQL endpoing with more than 10 keys is not supported.")
     (root/invoke* this
                   variables
                   (into [k1 k2 k3 k4 k5 k6 k7 k8 k9 k10] rest)))))

