(ns chia.routing.query-edn
  (:require [clojure.edn :as edn]
            [chia.routing.query :as q]
            [chia.util :as u]))

(defn try-read-str [x]
  (try (edn/read-string x)
       (catch :default _ x)))

(def encode pr-str)
(def decode try-read-str)

(defn assoc-query! [k v & kvs]
  (q/query-swap! merge (-> (apply hash-map k v kvs)
                           (u/update-vals encode))))

(defn update-query! [k f & args]
  (q/query-swap! update k (fn [v] (encode (apply f (decode v) args)))))

(defn get-query [& args]
  (let [res (apply q/get-query args)]
    (cond (map? res) (u/update-vals res decode)
          (string? res) (decode res)
          :else res)))