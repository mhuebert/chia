(ns triple.util.cond
  "Utilities related to conditional expressions"
  (:require [triple.util.macros :as m]))

(defmacro if-defined
  "like if-some but tests for `undefined?` (cljs-only)"
  ([binding then]
   `(if-defined ~binding ~then ~'js/undefined))
  ([binding then else]
   (let [[sym get-expr] binding]
     `(let [~sym ~get-expr]
        (if (~'cljs.core/undefined? ~sym)
          ~else
          ~then)))))

(defmacro when-defined
  ([binding & body]
   `(if-defined ~binding (do ~@body))))

(defn threaded
  [step-expr expr & forms]
  (let [g     (gensym)
        steps (map step-expr (repeat g) forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro guard-> [x expr]
  (threaded (fn [g step] `(if (-> ~g ~step) ~g))
            x expr))

(defmacro guard->> [x expr]
  (threaded (fn [g step] `(if (->> ~g ~step) ~g))
            x expr))