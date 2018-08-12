(ns chia.util)

(defmacro for-map [& body]
  `(->> (for ~@body)
        (apply merge)))