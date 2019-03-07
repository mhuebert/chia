(ns chia.view.registry)

(def ^:dynamic *current-view*
  "Tracks the currently-rendering component."
  nil)

(def ^:dynamic ^boolean *reload*
  "When true, all components re-render, regardless of shouldComponentUpdate."
  false)

(def reload-count
  "Increments at each reload. Use to invalidate caches."
  (volatile! 0))

(defn- ^:dev/before-load reload-count! []
  (vswap! reload-count inc))

(def instance-counter
  "For tracking the order in which components have been constructed (parent components are always constructed before their children)."
  (volatile! 0))

(defonce registry-ref (atom {}))

(defn register-view! [view-var]
  (when js/goog.DEBUG
    (let [{:as var-meta
           :keys [view/name
                  doc]} (meta view-var)]
      (swap! registry-ref assoc name
             (merge
              (select-keys var-meta [:view/name
                                     :view/arglist])
              (when doc {:doc doc})
              {:view @view-var})))))
