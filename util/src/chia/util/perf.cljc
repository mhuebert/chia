(ns chia.util.perf
  (:refer-clojure :exclude [str identical?])
  (:require [clojure.core :as core]
            [chia.util.macros :as m])
  #?(:cljs (:require-macros [chia.util.perf])))

(m/defmacro str
  [out x]
  (m/case :cljs `(let [out# ~out
                       x# ~x]
                   (~'js* "~{} += ~{}" out# x#))
          :clj `(core/str ~out ~x)))

(m/defmacro identical? [a b]
  (m/case :clj `(core/identical? ~a ~b)
          :cljs (if (keyword? a)
                  `(and (keyword? ~b)
                        (core/identical? (.-fqn ~a)
                                         (.-fqn ~b)))
                  `(core/identical? ~a ~b))))

(m/defmacro identical-in?
  "Returns true if `x` is identical to any item in `coll` (expands to sequential `identical?` comparisons)."
  [coll x]
  `(or ~@(for [option coll]
           `(identical? ~option ~x))))

(m/defmacro keyword-in?
  "Returns true if `x` is identical to any item in `coll` (expands to sequential `keyword-identical?` (cljs) or `identical?` (clj) comparisons)."
  [coll x]
  (let [fqn-sym (gensym "kw-fqn")]
    `(and (keyword? ~x)
          (let [~fqn-sym (~'.-fqn ~x)]
            (or ~@(for [option coll]
                    `(core/identical? (~'.-fqn ~option) ~fqn-sym)))))))

(m/defmacro unchecked-keyword-identical? [a b]
  `(core/identical? (.-fqn ~a) (.-fqn ~b)))

(m/defmacro condentical [k & pairs]
  (let [trailing-clause (when (odd? (count pairs))
                          (last pairs))]
    `(cond ~@(->> (partition 2 pairs)
                  (mapcat (fn [[test-k expr]]
                            (cond (or (keyword? test-k)
                                      (keyword? k))
                                  `((identical? ~k ~test-k) ~expr)
                                  (list? test-k)
                                  `((identical-in? ~test-k ~k) ~expr)
                                  :else `((core/identical? ~test-k ~k) ~expr)))))
           ~@(when trailing-clause
               `(:else ~trailing-clause)))))