(ns chia.util.js-interop
  (:refer-clojure :exclude [get get-in])
  (:require [clojure.core :as core]))

(defn kw-name [k]
  (cond-> k
          (keyword? k) (name)))

(defn- get*
  ([o k]
   (get* o k nil))
  ([o k not-found]
   `(~'goog.object/get ~o ~(kw-name k) ~not-found)))

(defmacro get
  [& args]
  (apply get* args))

(defn- get-in*
  ([o path]
   (get-in* o path nil))
  ([o path not-found]
   `(or ~(if (sequential? path)
           `(~'goog.object/getValueByKeys ~o ~@(mapv kw-name path))
           `(.apply ~'goog.object/getValueByKeys
                    nil
                    (to-array (cons ~o (seq ~path)))))
        ~not-found)))

(defmacro get-in
  [& args]
  (apply get-in* args))