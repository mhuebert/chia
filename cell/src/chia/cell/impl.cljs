(ns chia.cell.impl)

(defn- timeout
       ([n f] (timeout n f nil))
       ([n f initial-value]

        (let [self cell/*cell*
              _ (cell/status! self :loading)
              clear-key (js/setTimeout (cell-fn []
                                                (cell/status! self nil)
                                                (reset! self (f @self))) n)]
             (on-dispose self #(js/clearTimeout clear-key))
             initial-value)))