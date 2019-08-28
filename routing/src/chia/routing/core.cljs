(ns chia.routing.core
  (:refer-clojure :exclude [resolve])
  (:require [chia.routing :as routing]
            [chia.routing.aux :as aux]
            [chia.routing.routers :refer [db-id]]
            [chia.db :as db]
            [chia.util :as u]
            [kitchen-async.promise :as p]
            [chia.routing.registry :as registry]
            [chia.routing.resolve :as resolve]
            [chia.routing.bidi :as b]
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating paths

(defonce global-route-params (atom (constantly {})))

(defn- path-for* [router [handler params]]
  (b/path-for (registry/paths router) handler (merge (@global-route-params) params)))

(defn- path-string [router args]
  (cond (string? args) args
        (vector? args) (path-for* router args)
        (map? args) (:path args)))

(defn- normalize-path-arg [path-arg]
  (if (map? path-arg)
    path-arg
    (as-> path-arg path-arg

          ;; :some/handler > [:some/handler]
          (if (keyword? path-arg) [path-arg] path-arg)

          ;; [:some/handler] > {:router/x [:some/handler]}
          (if (vector? path-arg)
            ;; look up router for handler
            (let [handler-key (first path-arg)
                  router (registry/router-lookup handler-key)]
              {router path-arg})
            path-arg))))

(defn- path*
  [handler-or-routers]
  (->> (normalize-path-arg handler-or-routers)
       (reduce-kv (fn [m router args]
                    (if (nil? args)
                      m
                      (assoc m router (path-string router args))))
                  {})
       (aux/emit-routes {:ns "router"})))

(defn path
  ([path-arg]
   (if (string? path-arg)
     path-arg
     (let [{:as changed-routers
            :keys [query]} (normalize-path-arg path-arg)]
       (->> (merge (u/update-vals (db/entity db-id) :path)
                   changed-routers
                   {:query query})
            (path*)))))
  ([router args & more-routers]
   (path (->> (cons router (cons args more-routers))
              (apply hash-map)))))

(def nav! (comp routing/nav! path))
(def link? routing/link?)

(defonce listen!
         ;; Begin listening to changes in pushState routes
         (delay
          (routing/listen
           (fn [location]
             (p/let [routers (aux/parse-path {:ns "router"} (:path location))
                     resolved-routers (resolve/resolve-async routers)]
               (doseq [f (registry/listeners)]
                 (f resolved-routers)))))))

(defn replace-entity [id entity]
  (let [prev (db/entity id)]
    (into [(assoc entity :db/id id)]
          (for [k (set/difference (set (keys prev)) (set (keys entity)))
                :when (not= :db/id k)]
            [:db/retract-attr id k]))

    ))

(defn init! [{:as   routes
              :keys [listener
                     global-params]}]

  (some->> global-params (reset! global-route-params))

  (registry/add! routes)

  (when listener
    (registry/listen! ::init listener))

  (registry/listen! :chia/db
                    (fn [routers]
                      (db/transact! (replace-entity db-id routers))))



  @listen!)
