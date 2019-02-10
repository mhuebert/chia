(ns chia.view.hiccup
  (:require [chia.view.hiccup.impl :as hiccup]
            ["react" :as react]
            [chia.util.perf :as perf]
            [chia.util.js-interop :as j]))

(enable-console-print!)
(set! *warn-on-infer* true)

;; patch IPrintWithWriter to print javascript symbols without throwing errors
(when (exists? js/Symbol)
  (extend-protocol IPrintWithWriter
    js/Symbol
    (-pr-writer [sym writer _]
      (-write writer (str "\"" (.toString sym) "\"")))))

(defprotocol IElement
  (to-element [this] "Returns a React element representing `this`"))

(defprotocol IEmitHiccup
  (to-hiccup [this] "Returns a hiccup form representing `this`"))

(defn format-props [clj-props tag]
  (assert (keyword? tag)
          "The first element in a vector must be a keyword.")
  (let [[_ tag id classes] (hiccup/parse-key-memoized tag)]
    [tag (hiccup/props->js tag id classes clj-props)]))

(defn make-fragment [children]
  (.apply react/createElement nil
          (doto (to-array children)
            (.unshift nil)
            (.unshift react/Fragment))))

(defn element-arr [to-element form-vec]
  (reduce (fn [out form]
            (doto out (.push (to-element form)))) #js [] form-vec))



(defn -to-element [form]
  (when form
    (cond (vector? form)
          (let [tag (form 0)]
            (cond (keyword? tag)
                  (if (keyword-identical? :<> tag)
                    (make-fragment (element-arr -to-element (subvec form 1)))
                    (let [[props children] (hiccup/parse-args form)
                          [js-tag js-props] (format-props props (form 0))
                          args (hiccup/reduce-flatten-seqs -to-element [js-tag js-props] conj children)]
                      (apply react/createElement args)))
                  (fn? tag) (if (j/get tag :chia$functionalComponent)
                              (.call react/createElement nil tag nil (subvec form 1))
                              (-to-element (apply tag (rest form))))
                  :else (make-fragment (element-arr -to-element form))))

          (satisfies? IElement form)
          (to-element form)

          (satisfies? IEmitHiccup form)
          (-to-element (to-hiccup form))

          (seq? form)
          (.apply react/createElement nil
                  (reduce (fn [^js arr el]
                            (doto arr
                              (.push (-to-element el))))
                          #js [react/Fragment nil] form))

          (= js/Array (.-constructor form))
          (if (fn? (first form))
            (.apply react/createElement nil (hiccup/clj->js-args! form -to-element))
            (make-fragment form))

          :else form)))

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