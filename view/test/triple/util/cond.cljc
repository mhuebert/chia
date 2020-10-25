(ns triple.util.cond
  "Utilities related to conditional expressions"
  #?(:cljs (:require-macros [triple.util.cond :as cond])))

(defn guard [x f]
  (when (f x)
    x))

#?(:cljs
   (defn defined? [x]
     (not (undefined? x))))