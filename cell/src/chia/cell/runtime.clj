(ns chia.cell.runtime)

(defmacro with-runtime [runtime & body]
  `(let [child# ~runtime]
     (~'chia.cell.runtime/on-dispose #(~'chia.cell.runtime/dispose! child#))
     (binding [~'chia.cell.runtime/*runtime* child#]
       ~@body)))