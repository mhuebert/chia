(ns chia.cell.legacy)

(defn loading? [cell]
  (:async/loading? cell))

(defn error [cell]
  (:async/error cell))

(defn status [cell]
  (cond (:async/error cell) :error
        (:async/loading? cell) :loading
        :else nil))