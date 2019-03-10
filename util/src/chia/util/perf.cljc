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

(defn butlastv [v]
  (cond-> v
          (> (count v) 0) (pop)))