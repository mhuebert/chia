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