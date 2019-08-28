(ns chia.routing.registry
  (:require [chia.util.string :as u-str]
            [chia.lazy :as lazy]
            [kitchen-async.promise :as p]
            [bidi.bidi :as bidi]
            [cljs.pprint :as pp]))

(defonce *index (atom {}))
(defonce *listeners (atom {}))

;; Read

(defn paths [router]
  (or (get (:paths @*index) router)
      (throw (js/Error. (str "Router has no paths: " router)))))

(defn handler [k]
  (or (some-> (k (:handlers @*index))
              (deref))
      (throw (js/Error. (str "Handler not registered: " k)))))

(defn listeners []
  (vals @*listeners))

(defn router-lookup [handler-k]
  (or (handler-k (:router-lookup @*index))
      (throw (js/Error. (str "Cannot find router for handler: " handler-k)))))

;; Listen

(defn listen! [k f]
  (swap! *listeners assoc k f))

(defn unlisten! [k]
  (swap! *listeners dissoc k))

;; Register

(defn- normalize-paths [router-key paths]
  (let [f (if (= router-key :router/root)
            u-str/ensure-prefix
            u-str/trim-prefix)]
    (update paths 0 (partial f "/"))))

(defn merge-paths [p1 p2]
  (if p1
    (update p1 1 merge (nth p2 1))
    p2))

(def into-map (fnil into {}))

(defn- index-router [index [router-key {:keys [paths handlers]}]]
  (let [handler-keys (map :handler (bidi/route-seq paths))
        load-handler (fn [handler-k]
                       (delay
                         (p/-> (lazy/checked handlers)
                               (handler-k))))]
    (-> index
        (update :handlers into-map
                (map (juxt identity load-handler) handler-keys))
        (update-in [:paths router-key] merge-paths
                   (normalize-paths router-key paths))
        (update :router-lookup into-map
                (map (juxt identity (constantly router-key)) handler-keys)))))

(defn- with-fallback [index not-found]
  (cond-> index
          not-found (assoc-in [:handlers :handler/not-found] (delay not-found))))

(defn add! [{:keys [routers handler/not-found]}]
  (swap! *index
         (fn [index]
           (let [index (with-fallback index not-found)]
             (->> routers
                  (mapcat (fn [[router-key routers]]
                            (for [router (if (map? routers) [routers] routers)]
                              [router-key router])))
                  (reduce index-router index))))))