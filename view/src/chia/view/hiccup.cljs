(ns chia.view.hiccup
  (:require [chia.view.hiccup.impl :as hiccup]
            ["react" :as react]
            [chia.util.perf :as perf]
            [applied-science.js-interop :as j]))

(def -react-element react/createElement)
(def -react-fragment react/Fragment)
(def -react-element? react/isValidElement)

(defprotocol IElement
  (to-element [this] "Returns a React element representing `this`"))

(declare -to-element)

(defonce ^:private sentinel #js{})

(defn make-element
  [tag js-props form start]
  (let [form-count (count form)]
    (case (- form-count start)                              ;; fast cases for small numbers of children
      0 (-react-element tag js-props)
      1 (let [first-child (-nth form start)]
          (if (seq? first-child)
            ;; a single seq child should not create intermediate fragment
            (make-element tag js-props (vec first-child) 0)
            (-react-element tag js-props (-to-element first-child))))
      (let [out #js[tag js-props]]
        (loop [i start]
          (if (== i form-count)
            (.apply -react-element nil out)
            (do
              (.push out (-to-element (nth form i)))
              (recur (inc i)))))))))

(defn- make-fragment [children]
  (make-element -react-fragment nil children 1))

(defonce sentinel #js{})

(defn props? [props]
  (not (identical? props sentinel)))

(defn get-props
  "Returns props at index `i` in `form`, or a sentinel value if props were not found.
   Props can be `nil` or a Clojure map.
   You should call `props?` on the result to detect of there is a usable props map.
   Props will be nil, a map, or the sentinel 'not-props' value."
  [form i]
  {:post [(or (nil? %) (map? %) (identical? % sentinel))]}
  (let [props (-nth form i sentinel)]
    (if (identical? props sentinel)
      sentinel
      (if (or (nil? props) (map? props))
        props
        sentinel))))

(defn -to-element [form]
  {:pre [(not (keyword? form)) (not (map? form))]}
  (case (goog/typeOf form)
    "array" (if (fn? (aget form 0))
              (let [props (get-props form 1)
                    props? (props? props)]
                (make-element (aget form 0) (when props? (hiccup/props->js props)) form (if props? 2 1)))
              (make-fragment form))
    "object" (cond (not (identical? "object" (goog/typeOf form)))
                   form

                   (vector? form) (let [tag (-nth form 0)]
                                    (cond (keyword? tag)
                                          (let [props (get-props form 1)
                                                props? (props? props)]
                                            (if (perf/identical? :<> tag)
                                              (make-fragment form)
                                              (let [parsed-key (hiccup/parse-key-memo (name tag))]
                                                (make-element (.-tag parsed-key) (hiccup/props->js parsed-key (when props? props)) form (if props? 2 1)))))
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