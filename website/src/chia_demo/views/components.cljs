(ns chia-demo.views.components
  (:require [chia.view :as v]
            [chia.view.class :as legacy]
            ["@material-ui/core/Button" :default Button]))

(legacy/defclass my-method
  {:key :a}
  [this b c]
  [:div (str " a " (:a (:view/props this))
             " b " b
             " c " c)])

(legacy/defclass demo
  {:demo/title "Components"}
  [{:keys [view/state]}]
  [:div.pa2
   #js [Button {:on-click #(swap! state update :yes not)
                :variant "contained"
                :color "primary"} "switch!"]
   #js [Button #js {:variant "contained"} "...with js props"]
   #js [Button nil "...with nil props"]
   (my-method {:a "A"} "B" "C")
   [my-method {:a "AA"} "BB" "CC"]])
