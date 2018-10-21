(ns chia-demo.styles
  (:require [chia.material-ui :as m]
            [chia.util.js-interop :as j]
            [chia.jss :as jss]
            [chia.jss.functional-styles :as f-styles]))

(defonce theme (m/create-theme {}))

(def unit (j/get-in theme [:spacing :unit]))

(defonce base (f-styles/base {:unit unit}))

(def classes* (jss/make-classes
               (-> (f-styles/base {:unit unit})
                   (update "@global" merge
                           (f-styles/color-styles {:orange "#fa7921"
                                                   :light-orange "#fe9920"
                                                   :green "#2e933c"
                                                   :dark-green "#306b34"
                                                   :dark-blue "#0c4767"
                                                   :white "#ffffff"
                                                   :black "#000000"
                                                   :error-pink "#faeaed"})
                           {"body" {:font-family "'IBM Plex Sans', sans-serif"
                                    :font-size 15}
                            "div" {:tap-highlight-color "transparent"
                                   :-webkit-tap-highlight-color "transparent"}}))))
