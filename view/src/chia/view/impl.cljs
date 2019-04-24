(ns chia.view.impl
  (:require ["react" :as react]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as j]
            [chia.view.registry :as registry]))

;;;;;;;;;;;;;;
;;
;; Hook utils

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
;; ReactDOM

(def -render react-dom/render)
(def -unmount-component-at-node react-dom/unmountComponentAtNode)

(defn resolve-node [node-or-id]
  (cond->> node-or-id
           (string? node-or-id)
           (.getElementById js/document)))

;;;;;;;;;;;;;;
;;
;; Context

(defonce lookup-context
  ;; allow for lookup/creation of context by namespaced keyword
  (memoize
    (fn ^js [k]
      (if (object? k)
        k
        (do
          (assert (qualified-keyword? k)
                  (str "Context lookup keywords must be namespaced. [" (str k) "]"))
          (react/createContext))))))

;;;;;;;;;;;;;;
;;
;; View memoization
;;
;; We can memoize views by default in CLJS, thanks to immutable data / cheap equality checks.

(defn- args-not= [x y]
  (not= (j/get x :children)
        (j/get y :children)))

(def ^:private -memoize-view (if (fn? react/memo) react/memo identity))

(defn memoize-view
  "Returns a memoized version of view `f` with optional `should-update?` function.

  - By default, arguments are compared with cljs equality.
  - During dev reload, all components re-render.
  - A no-op in node.js"
  ([f]
   (memoize-view f args-not=))
  ([f should-update?]
   (-memoize-view f
                  (fn use-last-value? [x y]
                    (if registry/*reload*
                      false
                      (not (should-update? (j/get x :children)
                                           (j/get y :children))))))))


