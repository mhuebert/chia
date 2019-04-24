(ns chia.view.hiccup
  (:require [chia.view.hiccup.impl :as hiccup]
            ["react" :as react]
            [chia.util.perf :as perf]
            [applied-science.js-interop :as j]))


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

(defn vector-props [tag props]
  (let [[tag id classes] (hiccup/parse-key-memoized tag)]
    #js [tag (hiccup/props->js tag id classes props)]))

(defn valid-err [label x]
  (str label " are not valid hiccup elements: " x))

(declare -to-element)

(defn fragment [children]
  (->> children
       (reduce (fn [out el]
                 (j/push! out (-to-element el))) #js[react/Fragment nil])
       (.apply react/createElement nil)))

(defn -to-element [form]
  (when form
    (assert (not (keyword? form)) (valid-err "Keywords" form))
    (assert (not (map? form)) (valid-err "Maps" form))

    (cond (react/isValidElement form) form
          (vector? form)
          (let [tag (form 0)]
            (cond (keyword? tag)
                  (if (perf/identical? :<> tag)
                    (fragment (rest form))
                    (let [[tag props children] (hiccup/split-args form)]
                      (->> (hiccup/reduce-flatten-seqs -to-element (vector-props tag props) j/push! children)
                           (.apply react/createElement nil))))
                  (fn? tag) (-to-element (apply tag (rest form)))
                  :else (throw (ex-info "Invalid hiccup vector" {:form form}))))

          (satisfies? IElement form)
          (to-element form)

          (satisfies? IEmitHiccup form)
          (-to-element (to-hiccup form))

          (seq? form) (fragment form)

          (array? form)
          (if (fn? (first form))
            (.apply react/createElement nil (hiccup/clj->js-args! form -to-element))
            (fragment form))

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