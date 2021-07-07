(ns triple.util.memo
  (:require [applied-science.js-interop :as j]))

(defn by-string
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

(defn clear! [memoized-fn]
  (j/call memoized-fn :clear))