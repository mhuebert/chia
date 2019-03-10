(ns chia.view
  (:require ["react-dom" :as react-dom]
            ["react" :as react]

            [chia.view.hooks :as hooks]
            [chia.view.props :as props]
            [chia.view.impl :as impl]
            [chia.view.render-loop :as render-loop]
            [chia.view.registry :as registry]

            [chia.view.hiccup :as hiccup]
            [chia.view.hiccup.impl :as hiccup-impl]

            [chia.reactive :as r]
            [chia.util :as u]

            [applied-science.js-interop :as j])
  (:require-macros [chia.view]))

(defn element? [x]
  (and x (impl/-is-valid-element? x)))

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
                   (impl/-create-element #js {:value context-v} out)))))))

(def use-context "Returns binding for context `context-k` (context or keyword)"
  hooks/use-context)

;;;;;;;;;;;;;;;;;;
;;
;; Props & Conversions

(def merge-props
  "Merge props, concatenating :class props and merging styles."
  props/merge-props)

(def partial-props
  "Partially applies props to view. Keys will be merged with other props."
  props/partial-props)

(def to-element
  "Converts hiccup to React element."
  props/to-element)

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
  (impl/-create-portal (to-element element) (impl/resolve-node dom-node)))

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

;;;;;;;;;;;;;;;;;;
;;
;; JS/React interop

;; TODO
;; document this & verify behaviour w.r.t. children
(defn adapt-react-class
  ([the-class]
   (adapt-react-class nil the-class))
  ([options the-class]
   (fn [& args]
     (let [[props children] (if (or (map? (first args))
                                    (nil? (first args)))
                              [(first args) (rest args)]
                              [{} args])
           props (props/adapt-props options props)
           js-form (-> (cons props children)
                       (to-array)
                       (j/unshift! the-class))]
       (to-element js-form)))))
