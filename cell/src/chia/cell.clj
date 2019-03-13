(ns chia.cell
  (:require [chia.cell.util :as util])
  (:refer-clojure :exclude [bound-fn]))

(defmacro read-tx-key [cell k]
  `(let [cell# ~cell
         tx-cell# (~'chia.cell/tx-cell cell#)]
     (or (~'applied-science.js-interop/get tx-cell# ~k)
         (~'applied-science.js-interop/get cell# ~k))))

(defmacro assoc-tx-key! [cell k v]
  `(~'chia.cell/mutate-cell! ~cell
    (~'applied-science.js-interop/obj ~k ~v)))

(defmacro update-tx-key! [cell k f & args]
  `(let [cell# ~cell]
     (assoc-tx-key! cell# ~k
                    (~f (read-tx-key cell# ~k) ~@args))))

(def lib-bindings
  (reduce (fn [bindings sym]
            (into bindings [(symbol (name sym)) sym]))
          []
          '[chia.cell.lib/interval
            chia.cell.lib/fetch]))

(defmacro defcell
  "Defines a named cell."
  [the-name & body]
  (let [[docstring body] (if (string? (first body))
                           [(first body) (rest body)]
                           [nil body])
        [options body] (if (and (map? (first body)) (> (count body) 1))
                         [(first body) (rest body)]
                         [nil body])
        cell-name (keyword (str *ns*) (str the-name))]
    `(do
       (declare ~the-name)
       (let [prev-cell# ~the-name]
         (def ~the-name
           ~@(when docstring (list docstring))
           (let ~lib-bindings
             (cond-> (~'chia.cell/cell*
                      ~cell-name
                      (fn [~'self] ~@body)
                      (when prev-cell#
                        #::{:reload true
                            :prev-cell prev-cell#
                            :def? true}))
                     ~options (with-meta ~options))))))))



(defn- cell-identity
  "Construct a cell-name, incorporating the runtime-value of `key` if provided."
  [&env key]
  (let [namespace-segment `(str ~(str *ns*) #_#_"#" (hash ~'chia.view.registry/*view*)) #_(str *ns*)
        position (str "-" (util/unique-id)
                      "#L" (:line &env) "-C" (:column &env))]
    `(keyword ~namespace-segment
              (str (hash ~key)
                   ~position))))

(defmacro cell
  "Returns an anonymous cell. Only one cell will be returned per lexical instance of `cell`,
  unless a unique `key` is provided. Helper functions in `lib-bindings` (eg. interval) are
  hoisted into scope, as is `self`, which refers to the current cell."
  ([expr]
   `(~'chia.cell/cell nil ~expr))
  ([{:as options
     :keys [key]} & body]
   (assert (or (map? options)
               (nil? options)))
   (let [body (if (and (map? options) (seq body))
                body
                (cons options body))]
     `(let ~lib-bindings
        (~'chia.cell/cell* ~(cell-identity &env key)
         (fn [~'self] ~@body))))))

(defmacro bound-fn
  "Returns an anonymous function which will evaluate with the current cell in the stack.
  Similar to Clojure's `bound-fn`, but only cares about the currently bound cell."
  [& body]
  `(let [the-cell# (first ~'chia.cell/*stack*)
         context# ~'chia.cell.runtime/*runtime*]
     (fn [& args#]
       (binding [~'chia.cell/*stack* (cons the-cell# ~'chia.cell/*stack*)]
         (try (apply (fn ~@body) args#)
              (catch ~'js/Error error#
                (~'chia.cell.runtime/handle-error context# error#)))))))
