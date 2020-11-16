(ns chia.view.bench
  (:require [yawn.compiler :as c]))

(defmacro yawn [form]
  (c/compile form))

(tap> 1)
