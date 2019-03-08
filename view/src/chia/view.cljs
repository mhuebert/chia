(ns chia.view
  (:require ["react-dom" :as react-dom]
            ["react" :as react]

            [chia.reactive :as r]
            [chia.util :as u]

            [chia.view.render-loop :as render-loop]
            [chia.view.hiccup :as hiccup]
            [chia.view.hiccup.impl :as hiccup-impl]
            [chia.view.hooks :as hooks]
            [chia.view.props :as props]
            [chia.view.registry :as registry]

            [applied-science.js-interop :as j])
  (:require-macros [chia.view]))

(defn- resolve-node [node-or-id]
  (cond->> node-or-id
           (string? node-or-id)
           (.getElementById js/document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; React API

(def -create-element react/createElement)
(def -create-portal react-dom/createPortal)
(def -create-context react/createContext)
(def -is-valid-element? react/isValidElement)
(def -render react-dom/render)
(def -unmount-component-at-node react-dom/unmountComponentAtNode)
(def -forward-ref react/forwardRef)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Chia API

(defn element? [x]
  (and x (-is-valid-element? x)))

(def to-element props/to-element)
(def merge-props props/merge-props)

(defn portal
  [react-element dom-element]
  (-create-portal (to-element react-element)
                  (resolve-node dom-element)))

(defn render-to-dom
  "Render view to element, which should be a DOM element or id of element on page."
  ([react-element dom-element]
   (render-to-dom react-element dom-element nil))
  ([react-element dom-element {:keys [reload?]
                               :or   {reload? true}}]
   (binding [registry/*reload* reload?]
     (-render (to-element react-element)
              (resolve-node dom-element)))))

(defn unmount-from-dom
  [dom-element]
  (-unmount-component-at-node dom-element))

(defn on-unmount!
  "Register an unmount callback for `component`. This is not a hook - can be used anywhere/anytime."
  [^js this key f]
  (j/update! this :chia$onUnmount assoc key f))

;; Render loop

(def schedule-update render-loop/schedule-update!)
(def force-update! render-loop/force-update!)
(def flush! render-loop/flush!)

;;;;;;;;;;;;;;;;;;
;;
;; JS/React interop

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

