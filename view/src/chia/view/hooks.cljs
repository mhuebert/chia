(ns chia.view.hooks
  (:require ["react" :as react]
            [chia.view.render-loop :as render-loop]
            [chia.reactive :as r]
            [applied-science.js-interop :as j]
            [chia.view.registry :as registry]))


(def use-state* react/useState)
(def use-effect* react/useEffect)
(def use-context react/useContext)
(def use-reducer react/useReducer)
(def use-callback react/useCallback)
(def use-memo react/useMemo)
(def use-ref react/useRef)
(def use-imperative-handle react/useImperativeHandle)
(def use-layout-effect react/useLayoutEffect)
(def use-debug-value react/use-debug-value)

(def ^:private js-undefined (js* "void 0"))

(defn- wrap-effect [f]
  (fn []
    (let [destroy (f)]
      ;; we must return `undefined` (and not null) if there is no fn to call on dispose
      (if (fn? destroy)
        destroy
        js-undefined))))

(defn use-effect
  ([f]
   (use-effect* (wrap-effect f)))
  ([f memoize-by]
   (use-effect* (wrap-effect f)
                (cond-> memoize-by
                        (vector? memoize-by)
                        (to-array)))))

(deftype FunctionalView [chia$name
                         chia$order
                         chia$forceUpdate]
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "👁<" chia$name ">")))
  r/IReadReactively
  (-invalidate! [this _]
    (render-loop/schedule-update! this))
  render-loop/IForceUpdate
  (-force-update! [this] (chia$forceUpdate)))

(defn use-force-update! []
  (-> (use-reducer inc 0)
      (unchecked-get 1)))

(defn use-did-mount [f]
  (let [ref (use-ref false)]
    (when (false? (j/get ref :current))
      (j/assoc! ref :current true)
      (f))
    nil))

(defn use-will-unmount [f]
  (use-effect (constantly f) #js []))

(defn use-chia [view-name]
  (let [force-update! (use-force-update!)
        [chia$view _] (use-state* (fn [] (new FunctionalView
                                              view-name
                                              (vswap! registry/instance-counter inc)
                                              force-update!)))]
    (use-will-unmount
      (fn []
        (render-loop/forget! chia$view)
        (r/dispose-reader! chia$view)
        (doseq [f (some-> (j/get chia$view :chia$onUnmount)
                          (vals))]
          (f))))
    chia$view))

(defn use-state
  ([] (use-state nil))
  ([initial-state]
   (let [state-atom (-> (use-ref (atom initial-state))
                        (j/get :current))
         chia$view registry/*current-view*]
     (use-effect (fn []
                   (add-watch state-atom ::state-atom
                              (fn [_ _ old-state new-state]
                                (when (not= old-state new-state)
                                  (render-loop/schedule-update! chia$view))))
                   #(remove-watch state-atom ::state-atom)) #js [])
     state-atom)))

