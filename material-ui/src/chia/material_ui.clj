(ns chia.material-ui
  (:require [clojure.string :as str]
            [chia.util :as u]))

(defmacro defm
  ([the-name]
   `(~'chia.material-ui/defm ~the-name {}))
  ([the-name options]
   `(def ~the-name (~'chia.view/adapt-react-class (update ~options
                                                          :->js-keys conj :classes)
                    ~(-> the-name
                         (name)
                         (str/capitalize)
                         (u/camel-case)
                         (symbol))))))