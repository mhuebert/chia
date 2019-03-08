(ns chia.view.hooks
  "Adapt React hooks for ClojureScript"
  (:require ["react" :as react]
            [chia.view.render-loop :as render-loop]
            [chia.reactive :as r]
            [applied-science.js-interop :as j]
            [chia.view.registry :as registry]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Why does this namespace exist? why not just use React hooks directly?
;;
;; * some hooks that rely on javascript-specific semantics like `undefined` and js equality, these need to be adapted for cljs to work properly at all
;; * some behaviour can more succinct because cljs has macros, ie. we can provide a better api than JS with equivalent performance (eg. ref forwarding)
;; * some behaviour can be better-by-default in cljs because we use immutable data structures, like memoizing all components by default
;; * figwheel/shadow-style reloading needs to be explicitly supported
;; * built-in state handling is not consistent with how Clojure handles state (ie. use atoms)
;;
;; lastly, Chia has its own reactivity system which we want to support.

;;;;;;;;;;;;;;
;;
;; References to built-in hooks

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

;;;;;;;;;;;;;;
;;
;; Utils

(def ^:private js-undefined (js* "void 0"))

(defn- wrap-effect [f]
  (fn []
    (let [destroy (f)]
      ;; we must return `undefined` (and not null) if there is no fn to call on dispose
      (if (fn? destroy)
        destroy
        js-undefined))))

;;;;;;;;;;;;;;
;;
;; CLJS-friendly hooks

(defn use-effect
  "Like builtin `useEffect`. Wrapped so that `f` either returns a function or `undefined`, as required by useEffect."
  ([f]
   (use-effect* (wrap-effect f)))
  ([f memoize-by]
   (use-effect* (wrap-effect f)
                memoize-by)))

(defn use-force-update
  "Returns a `forceUpdate` function for the current view."
  []
  (-> (use-reducer inc 0)
      (aget 1)))

(defn use-memoized
  "Evaluates `f` once, caches and returns result. Unlike builtin `useMemo`, guaranteed to only evaluate once per lifecycle."
  ([f]
   (use-memoized ::key f))
  ([key f]
   (let [ref (use-ref ::unset)
         current (j/get ref :current)]
     (if (not= (j/get current :key) key)
       (let [value (f)]
         (j/assoc! ref :current #js {:value value
                                     :key   key})
         value)
       (j/get current :value)))))

(defn use-did-mount
  "Evaluates `f` on component mount, returns nil. For side effects."
  [f]
  (let [ref (use-ref false)]
    (when (false? (j/get ref :current))
      (j/assoc! ref :current true)
      (f))
    nil))

(defn use-will-unmount
  "Evaluates `f` when component unmounts."
  [f]
  (use-effect (constantly f) #js []))

(defn use-state
  "Returns an atom with `initial-state`. Current view will re-render when value of atom changes."
  ([] (use-state nil))
  ([initial-state]
   (let [chia$view registry/*current-view*
         state-atom (use-memoized (fn []
                                    (let [state-atom (atom initial-state)]
                                      (add-watch state-atom ::state-atom
                                                 (fn [_ _ old-state new-state]
                                                   (when (not= old-state new-state)
                                                     (render-loop/schedule-update! chia$view))))
                                      state-atom)))]
     (use-will-unmount #(remove-watch state-atom ::state-atom))
     state-atom)))

(defn use-forwarded-ref
  "Returns a `ref` which will be forwarded to parent. Requires `:view/forward-ref?` option on this view to be true."
  []
  (let [forwarded-ref (j/get registry/*current-view* .-chia$forwardRef)
        ref (use-ref)]
    (assert forwarded-ref "use-forwarded-ref requires :view/forward-ref? to be true")
    (use-imperative-handle forwarded-ref
                           (fn [] (j/get ref :current)))
    ref))

(defn use-interval
  [{:keys [interval
           now?
           key]} f]
  (let [effect (fn []
                 (when now? (f))
                 (let [i (js/setInterval f interval)]
                   #(js/clearInterval i)))]
    (use-effect effect #js[key interval])))

;;;;;;;;;;;;;;
;;
;; CLJS-friendly memoization

(defn memo
  "Returns a memoized version of view `f` with optional `should-update?` function.

  - By default, arguments are compared with cljs equality.
  - During dev reload, all components re-render."
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

;;;;;;;;;;;;;;
;;
;; Chia reactivity support

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

(defn use-chia* [view-name ^boolean ref]
  (let [force-update! (use-force-update)
        chia$view (use-memoized (fn []
                                  (cond-> (new FunctionalView
                                               view-name
                                               (vswap! registry/instance-counter inc)
                                               force-update!)
                                          (not (false? ref)) (j/assoc! .-chia$forwardRef ref))))]
    (use-will-unmount
      (fn []
        (render-loop/forget! chia$view)
        (r/dispose-reader! chia$view)
        (doseq [f (some-> (j/get chia$view :chia$onUnmount)
                          (vals))]
          (f))))
    chia$view))