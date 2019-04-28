(ns chia.view.hiccup
  (:require [chia.view.hiccup.impl :as hiccup]
            ["react" :as react]
            [chia.util.perf :as perf]
            [applied-science.js-interop :as j]))

(def -create-element react/createElement)
(def -fragment react/Fragment)
(def -valid-element? react/isValidElement)

(defprotocol IElement
  (to-element [this] "Returns a React element representing `this`"))

(declare -to-element)

(defn- make-element [tag js-props children first-child]
  ;; fast-path for small vectors - idea from reagent
  (case (- (count children) first-child)
    0 (-create-element tag js-props)
    1 (-create-element tag js-props
                       (-to-element (nth children first-child)))
    2 (-create-element tag js-props
                       (-to-element (nth children first-child))
                       (-to-element (nth children (inc first-child))))
    (->> children
         (reduce-kv (fn [out i el]
                      (cond-> out
                              (>= i first-child)
                              (j/push! (-to-element el)))) #js[tag js-props])
         (.apply -create-element nil))))

(defn- make-fragment [children]
  (make-element -fragment nil children 1))

(defn- -to-element [form]
  {:pre [(not (keyword? form)) (not (map? form))]}
  (case (goog/typeOf form)
    "array" (if (fn? (nth form 0))
              (let [props (nth form 1)
                    props? (or (nil? props) (map? props))]
                (make-element (nth form 0) (when props? (hiccup/props->js props)) form (if props? 2 1)))
              (make-fragment form))
    "object" (cond (not (identical? "object" (goog/typeOf form)))
                   form

                   (vector? form) (let [tag (nth form 0)
                                        props (nth form 1 nil)
                                        props? (or (nil? props) (map? props))]
                                    (cond (keyword? tag)
                                          (if (perf/identical? :<> tag)
                                            (make-fragment form)
                                            (let [parsed-key (hiccup/parse-key (name tag))]
                                              (make-element (.-tag parsed-key)
                                                            (hiccup/props->js parsed-key (when props? props))
                                                            form (if props? 2 1))))
                                          (fn? tag) (-to-element (apply tag (rest form)))
                                          :else (throw (ex-info "Invalid hiccup vector" {:form form}))))

                   (seq? form) (make-fragment (vec form))

                   (satisfies? IElement form) (to-element form)

                   :else form)
    form))

(defn element
  "Converts Hiccup form into a React element. If a non-vector form
   is supplied, it is returned untouched. Attribute and style keys
   are converted from `dashed-names` to `camelCase` as spec'd by React.

   - optional -
   :wrap-props (fn) is applied to all props maps during parsing.
   :create-element (fn) overrides React.createElement."
  ([form]
   (-to-element form))
  ([{:keys [wrap-props]} form]
   (binding [hiccup/*wrap-props* wrap-props]
     (-to-element form))))



;; patch IPrintWithWriter to print javascript symbols without throwing errors
(when (exists? js/Symbol)
  (extend-protocol IPrintWithWriter
    js/Symbol
    (-pr-writer [sym writer _]
      (-write writer (str "\"" (.toString sym) "\"")))))