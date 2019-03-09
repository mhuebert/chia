(ns chia-demo.views.components
  (:require [chia.view :as v]
            [chia.view.legacy :as legacy]
            ["@material-ui/core/Button" :default Button]))

(legacy/defview my-method
  {:key :a}
  [this b c]
  [:div (str " a " (:a (:view/props this))
             " b " b
             " c " c)])

(legacy/defview demo
  {:demo/title "Components"
   :view/styles {}}
  [{:keys [view/state]}]
  [:div.pa2
   #js [Button {:on-click #(swap! state update :yes not)
                :variant "contained"
                :color "primary"} "switch!"]
   #js [Button #js {:variant "contained"} "...with js props"]
   #js [Button nil "...with nil props"]
   (my-method {:a "A"} "B" "C")
   [my-method {:a "AA"} "BB" "CC"]])
