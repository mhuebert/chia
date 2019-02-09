(ns chia.view.context)

(defmacro consume [bindings & body]
  (loop [bindings (partition 2 bindings)
         out (cons 'do body)]
    (if-let [[ctx-sym ctx-k] (first bindings)]
      (recur (rest bindings)
             `(~'chia.view.context/consume*
               (~'chia.view.context/lookup-context ~ctx-k)
               (fn [~ctx-sym] ~out)))
      out)))

(defmacro use [bindings & body]
  (loop [bindings (partition 2 bindings)
         out (cons 'do body)]
    (if-let [[ctx-sym ctx-k] (first bindings)]
      (recur (rest bindings)
             `(let [~ctx-sym (-> ~ctx-k
                                 (~'chia.view.context/lookup-context)
                                 (~'chia.view.context/use-context))]
                ~out))
      out)))