(ns chia.cell.lib.impl
  (:require [chia.cell :as cell]))

(defn- timeout
       ([n f] (timeout n f nil))
       ([n f initial-value]

        (let [self (first cell/*stack*)
              _ (cell/status! self :loading)
              clear-key (js/setTimeout (cell/bound-fn []
                                                      (cell/status! self nil)
                                                      (reset! self (f @self))) n)]
             (on-dispose self #(js/clearTimeout clear-key))
             initial-value)))