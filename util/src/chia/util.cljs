(ns chia.util
  (:require [chia.util.js-interop])
  (:require-macros [chia.util]))

(defn guard [x f]
  (when (f x)
    x))

;; from https://github.com/clojure/core.incubator/blob/master/src/main/clojure/clojure/core/incubator.clj
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

;; modified from https://github.com/clojure/core.incubator/blob/master/src/main/clojure/clojure/core/incubator.clj
(defn disj-in
  "Dis[join]'s `value` from set at `path` returning a new nested structure.
   The set, if empty, and any empty maps that result, will not be present in the new structure."
  [m [k & ks :as path] value]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (disj-in nextmap ks value)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (let [new-set (disj (get m k) value)]
      (if (empty? new-set)
        (dissoc m k)
        (assoc m k new-set)))))

(defn update-keys [f m]
  (persistent!
   (reduce-kv (fn [m k v] (assoc! m (f k) v)) (transient (empty m)) m)))

(defn update-vals [f m]
  (persistent!
   (reduce-kv (fn [m k v] (assoc! m k (f v))) (transient (empty m)) m)))

(defn find-first [pred coll]
  (first (filter pred coll)))