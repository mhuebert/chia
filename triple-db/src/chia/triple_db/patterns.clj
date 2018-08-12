(ns chia.triple-db.patterns)

#_(defmacro capture-patterns
    "Evaluates body, returning map with evaluation result and read patterns."
    [& body]
    `(binding [~'chia.triple-db.patterns/*pattern-log* {}]
       (let [{value# :value
              tx-report# :tx-report} (~'chia.triple-db.core/db-log (do ~@body))
             patterns# ~'chia.triple-db.patterns/*pattern-log*]
         (~'chia.triple-db.core/notify-listeners tx-report#)
         {:value value#
          :patterns patterns#})))