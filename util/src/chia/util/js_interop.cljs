(ns chia.util.js-interop
  (:refer-clojure :exclude [get get-in assoc! assoc-in! update! update-in!])
  (:require [goog.object :as gobj])
  (:require-macros chia.util.js-interop))

(defn kw-name [k]
  (cond-> k
          (keyword? k) (name)))

(def get gobj/get)

(defn get-in
  ([obj ks]
   (.apply gobj/getValueByKeys nil (to-array (cons obj (mapv kw-name ks)))))
  ([obj ks not-found]
   (or (get-in obj ks) not-found)))

(defn assoc!
  [obj & pairs]
  (loop [[k v & more] pairs]
    (gobj/set obj (kw-name k) v)
    (if (seq more)
      (recur more)
      obj)))

(defn assoc-in! [obj ks value]
  (assert (> (count ks) 1))
  (let [ks (mapv kw-name ks)
        inner-obj (get-in obj (drop-last ks))]
    (gobj/set inner-obj (last ks) value)
    obj))

(defn update! [obj k f & args]
  (gobj/set obj (kw-name k)
            (apply f (cons (gobj/get obj (kw-name k)) args)))
  obj)

(defn update-in! [obj ks f & args]
  (let [ks (mapv kw-name ks)
        val-at-path (.apply gobj/getValueByKeys nil (to-array (cons obj ks)))]
    (assoc-in! obj ks (apply f (cons val-at-path args)))))

(defn &js
  "Allows for single-level destructuring of JS keys

   (let [{:keys [id]} (&js some-obj)]
     ...)"
  [obj]
  (when obj
    (reify
      ILookup
      (-lookup [_ k]
        (gobj/get obj (kw-name k)))
      (-lookup [_ k not-found]
        (gobj/get obj (kw-name k) not-found))
      IDeref
      (-deref [o] obj))))