(ns chia.static.shadow
  (:require [chia.static.assets :as a]))

(defmacro with-shadow-state [build-state & body]
  `(let [{:as build-state#
          config# :shadow.build/config} ~build-state]
     (binding [a/*output-dir* (:output-dir config#)
               a/*asset-path* (:asset-path config#)
               a/*content-hashes?* (= :release (:shadow.build/mode build-state#))]
       (do ~@body))))

(defn eval-if-fn [f] (if (fn? f) (f) f))

(defn- resolve-sym [sym]
  (require (symbol (namespace sym)))
  (resolve sym))

(defn- resolve-content [form]
  (cond (string? form) form
        (symbol? form) (eval-if-fn @(resolve-sym form))
        (list? form) (let [[f-sym & args] form]
                       (apply @(resolve-sym f-sym) args))
        :else (throw (ex-info "Content must be a string, symbol, or simple function call."
                              {:form form}))))

(defn write-assets!
  "Writes assets to the output-dir of the current shadow build.

   `assets` should be a map of {<path>, <form>}, where `form` will be evaluated.

   Intended for use in a shadow-cljs.edn config file."
  {:shadow.build/stage :flush}
  [build-state assets]
  (if (get-in build-state [::generated assets])
    build-state
    (with-shadow-state build-state
      (doseq [[path content] assets]
        (a/write-asset! path (resolve-content content)))
      (-> build-state
          (assoc-in [::generated assets] true)))))