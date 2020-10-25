(ns triple.view.react.context
  (:refer-clojure :exclude [use])
  (:require ["react" :as react]
            [applied-science.js-interop :as j]
            [triple.view.hiccup :as hiccup]
            [triple.util.memo :as memo])
  #?(:cljs (:require-macros [triple.view.react.context :as context])))

(def lookup-context*
  (memo/by-string
    (fn [context-name]
      (react/createContext))))

(defn lookup-context [k]
  (if (keyword? k)
    (lookup-context* k)
    k))

(defn use [context]
  (react/useContext (lookup-context context)))

(defn- provide-ctx [context value child]
  (-> (lookup-context context)
      (j/get :Provider)
      (react/createElement #js {:value value} child)))

(defn provide
  "Adds React contexts to the component tree.
   `bindings` should be a map of {<context>, <value>}
   where <context> is a namespaced keyword or React.createContext instance."
  [binding-map body]
  (loop [bindings (seq binding-map)
         out (hiccup/to-element body)]
    (if (empty? bindings)
      out
      (recur (rest bindings)
             (let [[context value] (first bindings)]
               (provide-ctx context value out))))))