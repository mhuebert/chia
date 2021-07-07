(ns triple.util.macros
  (:refer-clojure :exclude [case])
  #?(:cljs (:require-macros triple.util.macros)))

#?(:clj
   (defmacro case
     "Like reader-conditionals but run at compile-time,
      dispatches based on _target_ language"
     [& {:keys [cljs clj]}]
     (if (contains? &env '&env)
       `(if (:ns ~'&env) ~cljs ~clj)
       (if #?(:clj (:ns &env) :cljs true)
         cljs
         clj))))