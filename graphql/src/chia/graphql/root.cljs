(ns chia.graphql.root
  (:require [chia.graphql :as g]
            [chia.graphql.schema :as schema]
            [chia.util :as u]
            [chia.triple-db.core :as d]
            [applied-science.js-interop :as j]
            [chia.graphql.normalize :as n]
            [chia.graphql.request :as request]
            [kitchen-async.promise :as p]
            [chia.view :as v]
            [chia.graphql.exec :as exec]
            [clojure.string :as str]
            [cljs.pprint :as pp]
            [chia.view.registry :as registry]))

(defonce cache (d/create))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API Options

(def ^:private resolve-api-opts! (atom nil))

(defn set-api-options! [api-opts]
  (@resolve-api-opts! api-opts))

(defonce api-opts-promise (p/promise [resolve reject]
                            (reset! resolve-api-opts! resolve)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Root type

(def get-name (comp name :schema/type-key))
(def get-operation (comp keyword namespace :schema/type-key))

(defn root-id [root]
  {(get-name root) (hash (case (get-operation root)
                           :Mutation (-> root
                                         (select-keys [:schema/type-key
                                                       :root/view])
                                         #_(update :root/variables select-keys [:path]))
                           :Query (select-keys root [:root/xkeys
                                                     :root/variables])))})

(defn with-xkeys [root & ks]
  (assoc root :root/xkeys ks))

(defn with-options [root variables]
  (assoc root :root/options variables))

(defn update-state-atom! [root attrs]
  (when-let [a (-> root
                   :root/options
                   :async/state-atom)]
    (swap! a merge (u/select-ns attrs "async"))))

(defn merge-root-meta! [root attrs]
  (when-let [a (-> root
                   :root/options
                   :async/state-atom)]
    (swap! a merge attrs))
  (d/transact! cache [(-> attrs
                          (assoc :db/id (root-id root)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keep track of view<>root relationships

(defonce ^:private root-listeners (volatile! {}))

(defn- root-entry [root]
  (get @root-listeners (root-id root)))

(defn- maybe-unmount-root [root]
  (let [{:as   entry
         :keys [views
                lifecycle]} (root-entry root)]
    (when (empty? views)
      (doseq [on-unmount (-> lifecycle
                             :on-unmount
                             (vals))]
        (assert (fn? on-unmount))
        (on-unmount root))
      ;; keep cache around
      #_(d/transact! cache [[:db/retract-entity (root-id root)]]))))

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

(defn- mount-view! [root options view]
  (let [mount-root? (-> (root-entry root)
                        :views
                        (empty?))]

    (assert view)

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

(defn listen [root options view]
  (when-not (some-> (root-entry root)
                    :views
                    (contains? view))
    (mount-view! root options view)))

(defn form [root]
  (let [{:keys [root/xkeys
                schema/type-key]} root
        schema-entry (schema/entry type-key)
        operation-params (-> (u/update-keys (:args schema-entry)
                                            #(keyword (u/ensure-prefix (name %) "$")))
                             (assoc :graphql/operation (-> type-key
                                                           (namespace)
                                                           (str/lower-case))))
        field-params (->> (keys operation-params)
                          (reduce (fn [m k]
                                    (cond-> m
                                            (not (namespace k))
                                            (assoc (keyword (subs (name k) 1)) k))) {}))]
    [type-key operation-params
     `[~(keyword (:name schema-entry)) ~field-params ~@xkeys]]))

#_(defn exec!
    ([root variables & xkeys]
     (let [[variables xkeys] (if (map? variables)
                               [variables xkeys]
                               [{} (cons variables xkeys)])]
       (exec! (merge root #:root {:variables variables
                                  :xkeys     xkeys}))))
    ([{:as   root
       :keys [root/xkeys
              root/variables
              schema/type-key]}]
     (exec/exec {:form            (form root)
                 :xkeys           xkeys
                 :variables       variables
                 :schema/type-key type-key})))

(defn root-datoms [cache root payload]
  (let [{:keys [data
                errors
                time]} (j/lookup payload)
        form (form root)
        variables (:root/variables root {})
        lookup-keys (->> (g/children form)
                         (n/parse-keys nil variables)
                         (mapv second))
        datoms (when data
                 (n/form-datoms cache form variables data))]
    (when errors
      (let [{:keys [message path]} (j/lookup (aget errors 0))]
        (js/console.error (str "GraphQL: " (js->clj path) " " message))))

    (prn :gql/request time `(~(keyword (get-name root)) ~variables) (root-id root))

    (conj datoms {:db/id                    (root-id root)
                  :async/error              (js->clj errors :keywordize-keys true)
                  :async/loading?           false
                  :async/time               time
                  :graphql.root/lookup-keys lookup-keys
                  :graphql.root/variables   variables})))

(defn add-to-cache! [cache root-or-form payload]
  (->> (if (vector? root-or-form)
         (n/form-datoms cache root-or-form {} (exec/get-field payload :data))
         (root-datoms cache root-or-form payload))
       (d/transact! cache)))

(defn read-root
  ([root]
   (read-root cache root))
  ([cache root]
   (let [root-lookup-id (root-id root)]
     (when-let [{:as   root-entry
                 :keys [graphql.root/variables
                        graphql.root/lookup-keys]} (d/entity cache root-lookup-id)]
       (assert root "root-entry must contain root")
       (let [form (form root)
             value (n/read-keys* cache variables
                                 (d/select-keys cache :cache/root lookup-keys)
                                 (g/children form))]
         (merge value
                (select-keys root-entry [:async/loading?
                                         :async/error])))))))

(def read-fragment (partial n/read-fragment cache))

(defn write-root-response! [root payload]
  (add-to-cache! cache root payload)
  (read-root cache root))

(defn resolve!
  ([root] (resolve! nil root))
  ([{:as   options
     :keys [async/load-silently?]} root]
   (when-not load-silently?
     (merge-root-meta! root
                       {:async/loading? true}))
    #_(if (:resolve (schema/entry root))
        (exec! root))
   (-> (request/post! api-opts-promise (form root) (:root/variables root))
       (p/then (fn [response]
                 (let [result (write-root-response! root response)]
                   (merge-root-meta! root (u/select-ns result "async"))
                   result)))
       (p/catch* (fn [error]
                   (js/console.error error)
                   (prn :error-in-root root)
                   (merge-root-meta! root
                                     {:async/error {:message "Error sending GraphQL operation"
                                                    :error   error}}))))))

(defn group-options [m]
  (reduce-kv (fn [[variables options] k v]
               (case (namespace k)
                 ("graphql"
                   "async")
                 [variables (assoc options k v)]
                 [(assoc variables k v) options])) [{} {}] m))

(defmulti invoke-root get-operation)

(defn mutate! [root & [variables-options & xkeys]]
  (let [[variables-2 options-2] (group-options variables-options)]
    (resolve! (-> root
                  (cond-> (seq xkeys)
                          (assoc :root/xkeys xkeys))
                  (update :root/variables merge variables-2)
                  (update :root/options merge options-2)))))

(defmethod invoke-root :Mutation
  [root]

  (when-let [view registry/*current-view*]
    (listen root {} view))

  (assoc (read-root root)
    :gql/mutate! (partial mutate! root)))

(defmethod invoke-root :Query
  [root]

  (if-let [view registry/*current-view*]
    (listen root {:on-mount resolve!} registry/*current-view*)
    (resolve! root))

  ;; handle cache policies - :network-only, :cache-only, :cache-and-network, :cache-or-network
  (read-root root))

(defn invoke-root*
  ([this variables]
   (invoke-root* this variables (:root/xkeys this)))
  ([this variables xkeys]
   (invoke-root (merge this
                       #:root {:variables variables
                               :xkeys     xkeys
                               :view      registry/*current-view*}))))

#_(defn invoke [root variables & xkeys]
    (let [[variables xkeys] (if (map? variables)
                              [variables xkeys]
                              [{} (cons variables xkeys)])]
      (invoke-root (merg))))

(extend-type schema/Root
  IFn
  (-invoke
    ([this]
     (invoke-root* this
                   {}))
    ([this variables]
     (invoke-root* this
                   variables))
    ([this variables k1]
     (invoke-root* this
                   variables
                   [k1]))
    ([this variables k1 k2]
     (invoke-root* this
                   variables
                   [k1 k2]))
    ([this variables k1 k2 k3]
     (invoke-root* this
                   variables
                   [k1 k2 k3]))
    ([this variables k1 k2 k3 k4]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4]))
    ([this variables k1 k2 k3 k4 k5]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4 k5]))
    ([this variables k1 k2 k3 k4 k5 k6]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4 k5 k6]))
    ([this variables k1 k2 k3 k4 k5 k6 k7]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7 k8]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8 k9]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7 k8 k9]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8 k9 k10]
     (invoke-root* this
                   variables
                   [k1 k2 k3 k4 k5 k6 k7 k8 k9 k10]))
    ([this variables k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 rest]
     (assert (sequential? rest) "Calling a top-level GraphQL endpoing with more than 10 keys is not supported.")
     (invoke-root* this
                   variables
                   (into [k1 k2 k3 k4 k5 k6 k7 k8 k9 k10] rest)))))