(ns chia.view.hooks
  "React hooks in ClojureScript"
  (:require ["react" :as react]
            [chia.view.impl :as impl]
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
;; * figwheel/shadow-style reloading needs to be explicitly supported
;; * built-in state handling is not consistent with how Clojure handles state (ie. use atoms)
;;
;; lastly, Chia has its own reactivity system which we want to support.


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks

;; React hooks that have alternative implementations/wrappers in this ns
(def -use-state react/useState)
(def -use-effect react/useEffect)
(def -use-context react/useContext)
(def -use-memo react/useMemo)
(def -use-layout-effect react/useLayoutEffect)

;; React hooks to be used directly
(def use-reducer react/useReducer)
(def use-callback react/useCallback)
(def use-ref react/useRef)
(def use-imperative-handle react/useImperativeHandle)
(def use-debug-value react/use-debug-value)

(defn use-context
  "Returns binding for context `context-k` (context or keyword)"
  [context-k]
  (-use-context
   (impl/lookup-context context-k)))

(defn use-effect
  "`f` is called on every render, or each time `key` is not= to the previous `key`.

   If a function is returned from `f`, it will be called when the view unmounts."
  ([f]
   (-use-effect (impl/wrap-effect f)))
  ([f key]
   (let [key-ref (j/get (use-ref #js[key 0]) :current)
         key-count (let [kcount (aget key-ref 1)]
                     (if (not= key (aget key-ref 0))
                       (aset key-ref 1 (inc kcount))
                       kcount))]
     (aset key-ref 0 key)
     (-use-effect (impl/wrap-effect f)
                  #js [key-count]))))

(defn use-layout-effect
  "Like `use-effect` but called synchronously, after DOM operations are complete."
  ([f]
   (-use-layout-effect (impl/wrap-effect f)))
  ([f key]
   (let [key-ref (j/get (use-ref #js[key 0]) :current)
         key-count (let [kcount (aget key-ref 1)]
                     (if (not= key (aget key-ref 0))
                       (aset key-ref 1 (inc kcount))
                       kcount))]
     (aset key-ref 0 key)
     (-use-layout-effect (impl/wrap-effect f)
                         #js [key-count]))))

(defn use-will-unmount
  "Evaluates `f` when component unmounts."
  [f]
  (use-effect (constantly f) nil))

(defn use-memo
  "Evaluates `f` once, caches and returns result. Re-evaluates when `key` changes.

   Guaranteed to only evaluate once per lifecycle."
  ([f]
   (use-memo f ::key))
  ([f key]
   (assert (not (array? key))
           "use-memo `key` should not be a JavaScript array - rather, a primitive or Clojure data structure")
   (let [current (-> (use-ref #js[::unset nil])
                     (j/get :current))]
     (if (not= (aget current 0) key)
       (let [value (f)]
         (doto current
           (aset 0 key)
           (aset 1 value))
         value)
       (aget current 1)))))

(defn use-atom
  "Returns an atom with `initial-state`. Current view will re-render when value of atom changes."
  ([] (use-atom nil))
  ([initial-state]
   (let [chia$view registry/*view*
         state-atom (use-memo (fn []
                                (let [state-atom (atom initial-state)]
                                  (add-watch state-atom ::state-atom
                                             (fn [_ _ old-state new-state]
                                               (when (not= old-state new-state)
                                                 (render-loop/schedule-update! chia$view))))
                                  state-atom)))]
     (use-will-unmount #(remove-watch state-atom ::state-atom))
     state-atom)))

(defn use-force-update
  "Returns a `forceUpdate` function for the current view."
  []
  (-> (use-reducer inc 0)
      (aget 1)))

(defn use-did-mount
  "Evaluates `f` on component mount, returns nil. For side effects."
  [f]
  (let [ref (use-ref false)]
    (when (false? (j/get ref :current))
      (j/assoc! ref :current true)
      (f))
    nil))

(defn use-interval
  [{:keys [interval
           now?
           key]} f]
  (let [effect (fn []
                 (when now? (f))
                 (let [i (js/setInterval f interval)]
                   #(js/clearInterval i)))]
    (use-effect effect
                [key interval])))

(defn use-dom-ref
  "Returns a ref to be passed as the `:key` to a react view.
  When mounted, `f` is called once with the referenced DOM element."
  [f]
  (let [dom-ref (use-ref)]
    (use-layout-effect
     (fn []
       (f (j/get dom-ref :current)))
     nil)
    dom-ref))

;;;;;;;;;;;;;;;;;;;;;
;;
;; Chia-specific
;;

(defn use-forwarded-ref
  "Returns a `ref` which will be forwarded to parent.
  Requires `:view/forward-ref?` option on this view to be true."
  []
  (let [forwarded-ref (j/get registry/*view* .-chia$forwardRef)
        ref (use-ref)]
    (assert (j/contains? registry/*view* .-chia$forwardRef) "use-forwarded-ref requires :view/forward-ref? to be true")
    (use-imperative-handle forwarded-ref
                           (fn [] (j/get ref :current)))
    ref))