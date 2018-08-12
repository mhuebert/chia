(ns chia.material-ui
  (:require [clojure.string :as str]))

(defn dashed->camel [s]
  (-> (name s)
      (str/capitalize)
      (str/replace #"\-(\w)" (fn [[_ letter]] (str/upper-case letter)))))

(defmacro defm [the-name]
  `(def ~the-name (~'chia.view/adapt-react-class ~(-> the-name
                                                      (dashed->camel)
                                                      (symbol)))))