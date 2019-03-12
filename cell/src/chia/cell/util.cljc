(ns chia.cell.util
  #?(:cljs (:require [cljs-uuid-utils.core :as uuid-utils]
                     [applied-science.js-interop :as j])))

(defn unique-id []
  #?(:cljs (uuid-utils/make-random-uuid)
     :clj  (str (java.util.UUID/randomUUID))))

#?(:cljs
   (defn id [cell]
     (j/get cell .-id)))