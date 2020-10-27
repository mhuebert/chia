(ns chia.view.bench
  (:require [hicada.compiler :as c]))

(defmacro hicada [form]
  (c/compile form))

(tap> 1)
