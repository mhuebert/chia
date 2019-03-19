(ns chia.view
  (:require ["react-dom" :as react-dom]
            ["react" :as react]

            [chia.view.hooks :as hooks]
            [chia.view.props :as props]
            [chia.view.impl :as impl]
            [chia.view.render-loop :as render-loop]
            [chia.view.registry :as registry]
            [chia.view.util :as vu]

            [chia.view.hiccup :as hiccup]
            [chia.view.hiccup.impl :as hiccup-impl]

            [chia.reactive :as r]
            [chia.util :as u]

            [applied-science.js-interop :as j])
  (:require-macros [chia.view]))

;;;;;;;;;;;;;;
;;
;; React

(def -create-element react/createElement)
(def -create-portal react-dom/createPortal)
(def -create-context react/createContext)
(def -is-valid-element? react/isValidElement)
(def -forward-ref react/forwardRef)

(defn element? [x]
  (and x (-is-valid-element? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Context API

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
               (-> (impl/lookup-context context-k)
                   (j/get :Provider)
                   (-create-element #js {:value context-v} out)))))))

(def use-context hooks/use-context)

;;;;;;;;;;;;;;;;;;
;;
;; Props & Conversions

(def merge-props props/merge-props)
(def partial-props props/partial-props)
(def to-element props/to-element)

;;;;;;;;;;;;;;;;;;
;;
;; Core API

(defn render-to-dom
  "Render view to element, which should be a DOM element or id of element on page."
  ([react-element dom-element]
   (render-to-dom react-element dom-element nil))
  ([react-element dom-element {:keys [reload?]
                               :or {reload? true}}]
   (binding [registry/*reload* reload?]
     (impl/-render (to-element react-element)
                   (impl/resolve-node dom-element)))))

(def unmount-from-dom
  "Unmounts React view at given DOM node."
  impl/-unmount-component-at-node)

(defn portal
  "Mounts `element` at `dom-node` as React portal."
  [element dom-node]
  (-create-portal (to-element element) (impl/resolve-node dom-node)))

;;;;;;;;;;;;;;;;;;
;;
;; Lifecycle

(defn on-unmount!
  "Register an unmount callback for `component`. This is not a hook - can be used anywhere/anytime."
  [^js this key f]
  (j/update! this :chia$onUnmount assoc key f))

;;;;;;;;;;;;;;;;;;
;;
;; Render loop

(def flush!
  "Flush pending operations to DOM"
  render-loop/flush!)

(def force-update!
  "Force a component to update"
  render-loop/force-update!)

;;;;;;;;;;;;;;;;;;
;;
;; Vanilla React interop

(defn adapt-react-class
  "Wraps a vanilla React class so that it can be used like any other view.

  See props/adapt-props for options."
  ([the-class]
   (adapt-react-class nil the-class))
  ([options the-class]
   (fn [& args]
     (let [[props children] (vu/parse-args args)
           props (props/adapt-props options props)]
       (->> (reduce j/push! #js[the-class props] children)
            (to-element))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Internal - Chia reactivity + render loop

(deftype FunctionalView [chia$name
                         chia$order]
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "👁<" chia$name ">")))
  r/IReadReactively
  (-invalidate! [this _]
    (render-loop/schedule-update! this)))

(defn -use-chia [view-name ^boolean ref]
  (let [force-update! (hooks/use-force-update)
        chia$view (hooks/use-memo (fn []
                                    (cond-> (new FunctionalView
                                                 view-name
                                                 (vswap! registry/instance-counter inc))
                                            true (j/assoc! :forceUpdate force-update!)
                                            (not (::no-ref ref)) (j/assoc! .-chia$forwardRef ref))))]
    (hooks/use-will-unmount
     (fn []
       (render-loop/dequeue! chia$view)
       (r/dispose-reader! chia$view)
       (doseq [f (some-> (j/get chia$view :chia$onUnmount)
                         (vals))]
         (f))))
    chia$view))

(defn -functional-render [{:keys [view/should-update?]
                           view-name :view/name
                           view-fn :view/fn
                           ^boolean ref? :view/forward-ref?}]
  (-> (fn [props ref]
        (binding [registry/*view* (-use-chia view-name (if ref? ref ::no-ref))]
          (r/with-dependency-tracking! registry/*view*
                                       (props/to-element (apply view-fn (j/get props :children))))))
      (doto (js/Object.defineProperty "name" #js{:value view-name}))
      (cond-> ref? (-forward-ref))
      (impl/memoize-view should-update?)))