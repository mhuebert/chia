(ns chia.reactive.lifecycle)

(defmacro with-runtime [runtime & body]
  `(let [child# ~runtime]
     (~'chia.reactive.lifecycle/on-dispose #(~'chia.reactive.lifecycle/dispose! child#))
     (binding [~'chia.reactive.lifecycle/*owner* child#]
       ~@body)))