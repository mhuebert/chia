(ns chia.material-ui
  (:require [clojure.string :as str]
            [chia.util :as u]))

(defmacro defm
  ([the-name]
   `(~'chia.material-ui/defm ~the-name {}))
  ([the-name options]
   (let [class-sym (-> the-name
                       (name)
                       (str/capitalize)
                       (u/camel-case)
                       (symbol))
         options (-> options
                     (update :->js-keys (fnil conj []) :classes)
                     (update :lift-nses (fnil conj #{}) "material"))]
     `(~'defn ~the-name [& args#]
        (let [[props# children#] (~'chia.view/parse-args args#)
              props# (~'chia.view/adapt-props ~options props#)
              children# (some->> children#
                                 (mapv ~'chia.view/to-element))
              arga# (.concat (~'cljs.core/array ~class-sym props#) (~'to-array children#))]
          (.apply ~'chia.view/create-element nil arga#))))))