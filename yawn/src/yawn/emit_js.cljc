(ns yawn.emit-js
  (:require [clojure.string :as str]
            [yawn.util :as util]))

(declare literal->js)

(defn map-to-js [m]
  {:pre [(every? util/primitive? (keys m))]}
  (when (seq m)
    (let [kvs-str (->> (keys m)
                       (mapv #(str \' (literal->js %) "':~{}"))
                       (interpose ",")
                       (str/join))]
      (vary-meta
        (list* 'js* (str "{" kvs-str "}") (mapv literal->js (vals m)))
        assoc :tag 'object))))

(defn literal->js
  "Efficiently emit to literal JS form"
  [x]
  (cond
    (nil? x) x
    (keyword? x) (name x)
    (string? x) x
    (vector? x) (apply list 'cljs.core/array (mapv literal->js x))
    (map? x) (map-to-js x)
    :else x))

(defn join-strings [separator v]
  (let [strs (->> (repeat (count v) "~{}")
                  (interpose ",")
                  (apply str))]
    (with-meta (list* 'js* (str "[" strs "].join('" separator "')") v) {:tag 'string})))
