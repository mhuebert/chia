(ns yawn.macros-test
  (:require [yawn.util :as util]
            [yawn.macros :as ym]
            [yawn.macros-test-util :as macros-util]
            [clojure.string :as str])
  #?(:cljs (:require-macros yawn.macros-test)))

(defmacro join-strings-macro [v]
  (util/casetime
    :deftime (if (every? string? v)
               (str/join v)
               `(str/join ~v))
    :usetime (str/join v)))

(defmacro stage-info []
  (ym/stage-info))

(defmacro util-macro-fn []
  (macros-util/util-fn))

(defn util-fn []
  (macros-util/util-fn))

(comment
  (macroexpand '(join-strings-macro ["a" "b"]))
  (macroexpand '(join-strings-macro [a b]))

  )
