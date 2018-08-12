(ns chia.routing.util
  (:require [clojure.string :as str]
   #?(:clj
            [ring.util.codec :as ring-codec]))
  #?(:cljs (:import [goog Uri])))

(defn some-str [s]
  (when (and s
             (not= "" s))
    s))

(defn ensure-leading-char
  "Ensure that string `s` begins with `prefix`"
  [prefix s]
  (cond->> s
           (not (str/starts-with? s prefix)) (str prefix)))

(defn trim-leading-char
  "Remove leading occurrence of `ch`"
  [ch s]
  (cond-> s
          (= ch (first s)) (subs 1)))

(defn wrap
  [[left right] s]
  (str left s right))

(defn remove-empty
  "Remove empty values/strings from map"
  [m]
  (reduce-kv (fn [m k v]
               (cond-> m
                       (or (nil? v)
                           (= "" v)) (dissoc k))) m m))


;; TODO
;; when lambdaisland.uri releases its `normalize` branch, we can probably
;; get rid of these implementations, & not use ring-codec / Uri at all

(defn form-encode
  "Returns value as www-form-urlencoded string"
  [m]
  #?(:cljs
     (.. (.-QueryData Uri)
         (createFromMap (clj->js m))
         (toString))
     :clj
     (ring-codec/form-encode m)))

(defn form-decode
  "Returns map of decoded www-form-urlencoded data"
  [s]
  #?(:cljs (let [uri (Uri. (ensure-leading-char "?" s))
                 data (.getQueryData uri)]
             (->> (.getKeys data)
                  (reduce (fn [m k]
                            (cond-> m
                                    (some-str k) (assoc (keyword k) (.get data k)))) {})))
     :clj  (->> s
                (ring-codec/form-decode)
                (reduce-kv (fn [m k v]
                             (cond-> m
                                     (some-str k) (assoc (keyword k) v))) {}))))