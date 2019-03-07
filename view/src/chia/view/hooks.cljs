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
                memoize-by)))

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

(defn use-once
  "Like use-memo, but accepts a function and is guaranteed to be called only once."
  [f]
  (let [ref (use-ref ::unset)]
    (if (not= ::unset (j/get ref :current))
      (j/get ref :current)
      (do (j/assoc! ref :current (f))
          (j/get ref :current)))))

(defn use-chia* [view-name ^boolean ref]
  (let [force-update! (use-force-update!)
        chia$view (use-once (fn []
                              (cond-> (new FunctionalView
                                           view-name
                                           (vswap! registry/instance-counter inc)
                                           force-update!)
                                      (not (false? ref)) (j/assoc! .-chia$ref ref))))]
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
   (let [chia$view registry/*current-view*
         state-atom (use-once (fn []
                                (let [state-atom (atom initial-state)]
                                  (add-watch state-atom ::state-atom
                                             (fn [_ _ old-state new-state]
                                               (when (not= old-state new-state)
                                                 (render-loop/schedule-update! chia$view))))
                                  state-atom)))]
     (use-will-unmount #(remove-watch state-atom ::state-atom))
     state-atom)))

(defn use-forwarded-ref []
  (let [forwarded-ref (j/get registry/*current-view* .-chia$ref)
        ref (use-ref)]
    (assert forwarded-ref "Must set :view/forward-ref? for use-forwarded-ref")
    (use-imperative-handle forwarded-ref
                           (fn [] (j/get ref :current)))
    ref))

(defn memo
  ([f]
   (let [args-equal? (fn [x y]
                       (if registry/*reload*
                         false
                         (= (j/get x :children)
                            (j/get y :children))))]
     (react/memo f args-equal?)))
  ([f should-update?]
   (let [args-equal? (fn [x y]
                       (if registry/*reload*
                         false
                         (not (should-update? (j/get x :children)
                                              (j/get y :children)))))]
     (react/memo f args-equal?))))