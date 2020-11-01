(ns hicada.util
  #?(:cljs (:require [applied-science.js-interop :as j])))

(defn primitive?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (or (string? x)
      (keyword? x)
      (number? x)
      (nil? x)))

(defn guard [x f] (when (f x) x))

(defn memo-by-string
  "Memoizes f, using cljs object lookups (for perf)"
  [f]
  #?(:clj
     (memoize f)

     :cljs
     (let [state (j/lit {:value {}})]
       (-> (fn [^string x]
             (let [cache (j/!get state :value)]
               (j/!get cache x
                       (let [v (f x)]
                         (j/!set cache x v)
                         v))))

           ;; clear the memory
           (j/assoc! :clear #(j/!set state :value #js{}))))))

#?(:cljs
   (defn memo-clear! [memoized-fn]
     (j/call memoized-fn :clear)))
