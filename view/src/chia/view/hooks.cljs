(ns chia.view.hooks
  "Adapt React hooks for ClojureScript"
  (:require ["react" :as react]
            [chia.view.render-loop :as render-loop]
            [chia.reactive :as r]
            [applied-science.js-interop :as j]
            [chia.view.registry :as registry]
            [chia.view.hiccup :as hiccup]
            [chia.view.props :as props]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Why does this namespace exist? why not just use React hooks directly?
;;
;; * some hooks that rely on javascript-specific semantics like `undefined` and js equality,
;;   these need to be adapted for cljs to work properly at all
;; * some behaviour can more succinct because cljs has macros, ie. we can provide a better 
;;   api than JS with equivalent performance (eg. ref forwarding)
;; * some behaviour can be better-by-default in cljs because we use immutable data structures, 
;;   like memoizing all components by default
;; * figwheel/shadow-style reloading needs to be explicitly supported
;; * built-in state handling is not consistent with how Clojure handles state (ie. use atoms)
;;
;; lastly, Chia has its own reactivity system which we want to support.

;;;;;;;;;;;;;;
;;
;; References to built-in hooks

(def use-state* react/useState)
(def use-effect* react/useEffect)
(def use-context* react/useContext)
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
                                     :key key})
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
    (assert (j/contains? registry/*current-view* .-chia$forwardRef) "use-forwarded-ref requires :view/forward-ref? to be true")
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



(defn- args-not= [x y]
  (not= (j/get x :children)
        (j/get y :children)))

(def -memo (if (fn? react/memo) react/memo identity))

(defn memo
  "Returns a memoized version of view `f` with optional `should-update?` function.

  - By default, arguments are compared with cljs equality.
  - During dev reload, all components re-render.
  - A no-op in node.js"
  ([f]
   (memo f args-not=))
  ([f should-update?]
   (-memo f
          (fn use-last-value? [x y]
            (if registry/*reload*
              false
              (not (should-update? (j/get x :children)
                                   (j/get y :children))))))))

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
                                          (not (::no-ref ref)) (j/assoc! .-chia$forwardRef ref))))]
    (use-will-unmount
     (fn []
       (render-loop/forget! chia$view)
       (r/dispose-reader! chia$view)
       (doseq [f (some-> (j/get chia$view :chia$onUnmount)
                         (vals))]
         (f))))
    chia$view))

;; internal (but not private - used by the v/defn macro)

(defn functional-render [{:keys [view/should-update?]
                          view-name :view/name
                          view-fn :view/fn
                          ^boolean ref? :view/forward-ref?}]
  (-> (fn [props ref]
        (binding [registry/*current-view* (use-chia* view-name (if ref? ref ::no-ref))]
          (r/with-dependency-tracking! registry/*current-view*
                                       (hiccup/element {:wrap-props props/wrap-props}
                                                       (apply view-fn (j/get props :children))))))
      (cond-> ref? (react/forwardRef))
      (memo should-update?)))

;;;;;;;;;;;;;;
;;
;; Contexts

(def -create-context react/createContext)

(defonce lookup-context
         (memoize
          (fn ^js [k]
            (if (object? k)
              k
              (-create-context)))))

(defn use-context [context-k]
  (use-context* (lookup-context context-k)))

(defn provide
  "Adds React contexts to the component tree.
   `bindings` should be a map of {<keyword-or-Context>, <value-to-be-bound>}."
  [binding-map & body]
  (loop [bindings (seq binding-map)
         out (props/to-element (vec (cons :<> body)))]
    (if (empty? bindings)
      out
      (recur (rest bindings)
             (let [[context-k context-v] (first bindings)]
               (-> (lookup-context context-k)
                   (j/get :Provider)
                   (react/createElement #js {:value context-v} out)))))))
