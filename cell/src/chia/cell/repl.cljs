(ns chia.cell.repl
  (:require [chia.cell :as cell]
            [chia.cell.deps :as deps]
            [com.stuartsierra.dependency :as dep]))

(defn reset-namespace
  "Purges and removes all cells in the provided namespace."
  [ns]
  (let [ns (str ns)
        cells (->> (:cells @cell/*graph*)
                   (keep (fn [[k data]]
                           (when (= (namespace k) ns)
                             (:instance data)))))]
    (doseq [cell (deps/topo-sort @cell/*graph* cells)]
      (cell/purge-cell! cell)
      (swap! cell/*graph* dep/remove-all cell))))