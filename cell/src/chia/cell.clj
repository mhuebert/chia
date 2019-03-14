(ns chia.cell
  (:require [chia.cell.util :as util])
  (:refer-clojure :exclude [bound-fn assoc! read]))

(def jget 'applied-science.js-interop/get)
(def jget-in 'applied-science.js-interop/get-in)

(defn read* [cell k not-found]
  `(let [cell# ~cell
         tx-cell# (~'chia.cell/tx-cell cell#)]
     (~jget tx-cell# ~k
      (~jget-in cell# [~'.-state ~k] ~not-found))))

(defmacro read
  ([cell k]
   (read* cell k nil))
  ([cell k not-found]
   (read* cell k not-found)))

(defmacro assoc! [cell k v]
  `(~'chia.cell/mutate-cell! ~cell
    (~'applied-science.js-interop/obj ~k ~v)))

(defmacro update! [cell k f & args]
  `(let [cell# ~cell]
     (assoc! cell# ~k
             (~f (read cell# ~k) ~@args))))

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
                         [nil body])]
    `(do
       (declare ~the-name)
       (let [prev-cell# ~the-name]
         (def ~(with-meta the-name options)
           ~@(when docstring (list docstring))
           (let ~lib-bindings
             (~'chia.cell/cell*
              (fn [~'self] ~@body)
              {:def? true
               :prev-cell prev-cell#})))))))

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
                (cons options body))
         id (util/unique-id)]
     `(let ~lib-bindings
        (~'chia.cell/cell*
         (fn [~'self] ~@body)
         {:memo-key (str ~id "#" (hash ~key))})))))

(defmacro bound-fn
  "Returns an anonymous function which will evaluate with the current cell in the stack.
  Similar to Clojure's `bound-fn`, but only cares about the currently bound cell."
  [& body]
  `(let [the-cell# ~'chia.cell/*cell*]
     (fn [& args#]
       (binding [~'chia.cell/*cell* the-cell#]
         (try (apply (fn ~@body) args#)
              (catch ~'js/Error e#
                (~'chia.cell/error! the-cell# e#)))))))
