(ns chia.material-ui
  (:require [clojure.string :as str]
            [chia.util :as u]))

(defmacro defm
  ([the-name]
   `(~'chia.material-ui/defm ~the-name {}))
  ([the-name options]
   `(def ~the-name (~'chia.view/adapt-react-class (-> ~options
                                                      (update :->js-keys conj :classes)
                                                      (update :lift-nses (fnil conj #{}) "material"))
                    ~(-> the-name
                         (name)
                         (str/capitalize)
                         (u/camel-case)
                         (symbol))))))