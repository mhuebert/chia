(ns chia.lazy)

(defmacro loadable [sym]
  `(~'chia.lazy/checked (~'shadow.lazy/loadable ~sym)))