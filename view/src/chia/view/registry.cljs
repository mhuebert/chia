(ns chia.view.registry)

(def ^:dynamic ^boolean *reload*
  "When true, all components re-render, regardless of shouldComponentUpdate."
  false)

(def reload-count
  "Increments at each reload. Use to invalidate caches."
  (volatile! 0))

(defn- ^:dev/before-load reload-count! []
  (vswap! reload-count inc))


