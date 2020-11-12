(ns test.hicada.macros-test
  (:require [hicada.util :as util]
            [clojure.string :as str])
  #?(:cljs (:require-macros test.hicada.macros-test)))

(defn join-strings-impl [v]
  (util/casetime
    :deftime (if (every? string? v)
               (str/join v)
               `(str/join ~v))
    :usetime (str/join v)))

(defmacro join-strings-macro [v]
  (join-strings-impl v))

(comment
  (macroexpand '(join-strings-macro ["a" "b"]))
  (macroexpand '(join-strings-macro [a b]))


  )
