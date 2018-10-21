(ns chia.util.js-interop
  (:refer-clojure :exclude [get get-in assoc! contains?])
  (:require [clojure.core :as core]))

(defn wrap-key [k]
  (cond
    (string? k) k
    (keyword? k) (name k)
    (symbol? k) (if (= (:tag (meta k)) "String")
                  k
                  `(wrap-key ~k))
    :else `(wrap-key ~k)))

(defn wrap-path [p]
  (if (vector? p)
    (mapv wrap-key p)
    `(mapv wrap-key ~p)))

(defn- get*
  ([o k]
   (get* o k nil))
  ([o k not-found]
   `(~'goog.object/get ~o ~(wrap-key k) ~not-found)))

(defmacro get
  [& args]
  (apply get* args))

(defn- get-in*
  ([o path]
   (get-in* o path nil))
  ([o path not-found]
   `(or ~(if (vector? path)
           `(~'goog.object/getValueByKeys ~o ~@(mapv wrap-key path))
           `(.apply ~'goog.object/getValueByKeys
                    nil
                    (to-array (cons ~o (map wrap-key ~path)))))
        ~not-found)))

(defmacro get-in
  [& args]
  (apply get-in* args))

(defmacro assoc! [o & pairs]
  `(doto ~o
     ~@(loop [pairs (partition 2 pairs)
              out []]
         (if (empty? pairs)
           out
           (let [[k v] (first pairs)]
             (recur (rest pairs)
                    (conj out `(~'goog.object/set ~(wrap-key k) ~v))))))))

(defmacro push! [a v]
  `(doto ~a
     (~'.push ~v)))

(defmacro unshift! [arr v]
  `(doto ~arr
     (~'.unshift ~v)))

(defmacro then [promise arglist & body]
  `(~'.then ~'^js ~promise
    (fn ~arglist ~@body)))

(defn contains? [o k]
  `(~'goog.object/containsKey o ~(wrap-key k)))

(defmacro call [o k & args]
  `(let [^js f# (get ~o ~k)]
     (~'.call f# ~o ~@args)))