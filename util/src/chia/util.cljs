(ns chia.util
  (:require-macros [chia.util]))

(defn guard [x f]
  (when (f x)
    x))