(ns chia.material-ui
  (:require [clojure.string :as str]
            [chia.util :as u]))

(defmacro defm
  ([the-name]
   `(~'chia.material-ui/defm ~the-name {}))
  ([the-name options]
   (let [class-name (-> the-name
                        (name)
                        (str/capitalize)
                        (u/camel-case)
                        (symbol))]
     `(def ~the-name
        (~'chia.material-ui/wrap-class ~class-name ~options)))))