(ns triple.util.string
  (:require [applied-science.js-interop :as j]
            [clojure.string :as str]
            [triple.util.memo :as memo]))

(defn -camel-case [s]
  (str/replace s #"-(.)" (fn [[_ s]] (str/upper-case s))))

(def camel-case (memo/by-string -camel-case))

(defn ensure-prefix [s pfx]
  (cond->> s
           (not (str/starts-with? s pfx)) (str pfx)))

(defn trim-prefix [s prefix]
  (cond-> s
          (str/starts-with? s prefix) (subs (count prefix))))

(defn replace-pattern [s pattern rep]
  #?(:clj (str/replace s pattern rep)
     :cljs (.replace s (j/!set pattern :lastIndex 0) rep)))

(defn split-pattern [s pattern]
  #?(:clj (str/split s pattern)
     :cljs (.split s pattern)))