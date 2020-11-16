(ns hicada.macros
  (:refer-clojure :exclude [for map into into-array])
  (:require [net.cgrand.xforms :as x]
            [clojure.core :as core]
            #?(:clj [net.cgrand.macrovich :as macros]))
  #?(:cljs (:require-macros [hicada.macros :as m]
                            [net.cgrand.macrovich :as macros])))

(def push! (completing #?(:cljs (fn [^js/Array arr x] (doto arr (.push x)))
                          :clj  conj)))

(defmacro transduce-arr [xform coll]
  `(transduce ~xform
              ~(macros/case :cljs 'hicada.macros/push!
                            :clj `conj)
              ~(macros/case :cljs '(cljs.core/array)
                            :clj [])
              ~coll))

#?(:cljs
   (defn into-array
         ([xform coll] (into-array (cljs.core/array) xform coll))
         ([init xform coll]
          (transduce xform push! init coll))))

(defmacro for
  ([bindings body]
   (when (seq bindings)
     (let [coll (nth bindings 1)
            into-form (:into (apply hash-map bindings))
           bindings (-> bindings
                        ;; (update 0 vary-meta assoc :tag 'any) ;; avoid type inference bug
                        (assoc 1 (symbol "%"))
                        (->> (partition 2)
                             (remove (comp #{:into} first))
                             (apply concat)
                             vec))]
       `(transduce-arr (x/for ~bindings ~body) ~coll)))))

(defmacro map
  [f coll]
  `(transduce-arr
     (clojure.core/map ~f)
     ~coll))

(comment

  (m/map (partial * 3) [1 2 3])
  (m/for [x [1 2 3]]
         (* 3 x))
  (into (array "...") (clojure.core/map #(* % 3)) [1 2 3]))
