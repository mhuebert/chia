(ns chia.util.js-interop
  (:refer-clojure :exclude [get unchecked-get get-in assoc! assoc-in! update! update-in! select-keys contains?])
  (:require [goog.object :as gobj]
            [cljs.core :as core])
  (:require-macros [chia.util.js-interop :as j]))

(defn wrap-key [k]
  (cond-> k
          (keyword? k) (name)))

(defn get
  ([o k]
   (j/get o k))
  ([o k not-found]
   (j/get o k not-found)))

(defn get-in
  ([obj ks]
   (.apply gobj/getValueByKeys nil (to-array (cons obj (map wrap-key ks)))))
  ([obj ks not-found]
   (or (get-in obj ks) not-found)))

(defn assoc!
  [obj & pairs]
  (loop [[k v & more] pairs]
    (gobj/set obj (wrap-key k) v)
    (if (seq more)
      (recur more)
      obj)))

(defn assoc-in! [obj ks value]
  (assert (> (count ks) 1))
  (let [ks (mapv wrap-key ks)
        inner-obj (get-in obj (drop-last ks))]
    (gobj/set inner-obj (last ks) value)
    obj))

(defn set! [obj k val]
  (core/unchecked-set obj (wrap-key k) val)
  obj)

(defn update! [obj k f & args]
  (gobj/set obj (wrap-key k)
            (apply f (cons (gobj/get obj (wrap-key k)) args)))
  obj)

(defn update-in! [obj ks f & args]
  (let [ks (mapv wrap-key ks)
        val-at-path (.apply gobj/getValueByKeys nil (to-array (cons obj ks)))]
    (assoc-in! obj ks (apply f (cons val-at-path args)))))

(deftype JSLookup [obj]
  ILookup
  (-lookup [_ k]
    (gobj/get obj (wrap-key k)))
  (-lookup [_ k not-found]
    (gobj/get obj (wrap-key k) not-found))
  IDeref
  (-deref [o] obj))

(defn lookup
  "Allows for single-level destructuring of JS keys

   (let [{:keys [id]} (lookup some-obj)]
     ...)"
  [obj]
  (JSLookup. obj))

(defn select-keys [o ks]
  (reduce (fn [m k]
            (let [k (wrap-key k)]
              (cond-> m
                      (gobj/containsKey o k)
                      (doto
                        (unchecked-set k
                                       (gobj/get o k nil)))))) #js {} ks))

(defn push! [^js a v]
  (doto a
    (.push v)))

(defn contains? [o k]
  (gobj/containsKey o (wrap-key k)))

(defn call [^js o k & args]
  (.apply (j/get o k) o (to-array args)))

(defn !get [o k]
  (core/unchecked-get o (wrap-key k)))