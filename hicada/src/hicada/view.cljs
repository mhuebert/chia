(ns hicada.view
  (:require [applied-science.js-interop :as j]
            hicada.interpreter)
  (:require-macros hicada.view))

(def ^:constant refresh-enabled?
  (and goog/DEBUG (exists? js/ReactRefresh)))

(defn register!
      "Registers a component with the React Fresh runtime.
      `type` is the component function, and `id` is the unique ID assigned to it
      (e.g. component name) for cache invalidation."
      [type id]
      (when refresh-enabled?
            (j/call js/ReactRefresh :register type id)))

(defn signature-fn []
      (when refresh-enabled?
            (j/call js/ReactRefresh :createSignatureFunctionForTransform)))
