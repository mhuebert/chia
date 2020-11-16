(ns yawn.macros-test
  (:require [yawn.util :as util]
            [clojure.string :as str]
            [cljs.analyzer :as ana])
  #?(:cljs (:require-macros yawn.macros-test)))

(defmacro join-strings-macro [v]
  (util/casetime
    :deftime (if (every? string? v)
               (str/join v)
               `(str/join ~v))
    :usetime (str/join v)))

(comment
  (macroexpand '(join-strings-macro ["a" "b"]))
  (macroexpand '(join-strings-macro [a b]))

  )
