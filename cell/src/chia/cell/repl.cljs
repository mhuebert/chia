(ns chia.cell.repl
  (:require [chia.cell :as cell]
            [chia.cell.deps :as deps]
            [com.stuartsierra.dependency :as dep]))

(defn reset-namespace
  "Purges and removes all cells in the provided namespace."
  [ns]
  (let [ns (str ns)
        cell-ids (->> (:cells @cell/*graph*)
                      (keys)
                      (filter #(= (namespace %) ns)))]
    (doseq [id (deps/topo-sort @cell/*graph* cell-ids)]
      (cell/purge-cell! id)
      (swap! cell/*graph* dep/remove-all id))))