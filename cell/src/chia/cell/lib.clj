(ns chia.cell.lib)

(defmacro timeout
  "Returns cell with body wrapped in timeout of n milliseconds."
  [n & body]
  `(~'chia.cell.lib.impl/timeout ~n (fn [] ~@body)))


