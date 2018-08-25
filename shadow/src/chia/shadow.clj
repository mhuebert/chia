(ns chia.shadow
  (:require [chia.static.assets :as a]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :refer [sh]]))

(defmacro with-shadow-state [build-state & body]
  `(let [{:as build-state#
          config# :shadow.build/config} ~build-state]
     (binding [a/*output-dir* (:output-dir config#)
               a/*asset-path* (:asset-path config#)
               a/*content-hashes?* (= :release (:shadow.build/mode build-state#))]
       (do ~@body))))

(defn- resource [path]
  (let [the-resource (-> (io/resource path)
                         (slurp))]
    (println ::resource (str path))
    the-resource))

(def aliased-fns {'resource resource})

(defn eval-if-fn [f] (if (fn? f) (f) f))

(defn- resolve-sym [sym]
  (or (get aliased-fns sym)
      (do
        (require (symbol (namespace sym)) :reload)
        @(resolve sym))))

(defn- simple-eval [form]
  (cond (string? form) form
        (symbol? form) (eval-if-fn (resolve-sym form))
        (list? form) (let [[f-sym & args] form]
                       (apply (resolve-sym f-sym) args))
        :else (throw (ex-info "Content must be a string, symbol, or simple function call."
                              {:form form}))))

(defn write-assets!
  "Writes assets to the output-dir of the current shadow build.

   `assets` should be a map of {<path>, <form>}, where `form` will be evaluated.

   Intended for use in a shadow-cljs.edn config file."
  {:shadow.build/stage :flush}
  [build-state assets]
  (if (and (get-in build-state [::generated assets])
           (not (:always? assets)))
    build-state
    (with-shadow-state build-state
                       (doseq [[path value] (dissoc assets :always?)]
                         (a/write-asset! path (simple-eval value)))
                       (-> build-state
                           (assoc-in [::generated assets] true)))))

(defn closure-defines
  {:shadow.build/stage :compile-prepare}
  [build-state vars]
  (update-in build-state [:compiler-options :closure-defines]
             merge (reduce-kv (fn [m var-name value]
                                (assoc m var-name (simple-eval value))) {} vars)))
