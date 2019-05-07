(ns chia-demo.views.context
  (:require [chia.view :as v]
            [chia.view.class :as class]
            [chia.reactive :as r]
            [chia.view.registry :as registry]))

(defn counter []
  (let [{:keys [view/state]} r/*reader*]
    (r/silently
     (swap! state update :count (fnil inc 0)))
    [:div (str "Renders:" (:count @state 0))]))

(defn clicks []
  (let [{:keys [view/state]} r/*reader*]
    [:a.db {:on-click #(swap! state update :clicks (fnil inc 0))}
     (str "Clicks:" (:clicks @state 0))]))

(v/defclass demo
  {:demo/title "React Contexts"
   :view/initial-state {:a "A"
                        :b "B"
                        :c "C"}}
  [{:keys [view/state]}]
  (let [mutate-context! (fn [k]
                          [:a.db {:on-click #(swap! state update k (fn [x] (str x (first x))))} (str "change!")])
        show-consumer (fn [k v]
                        [:div.pa3.lh-copy
                         [:.b (str k)]
                         (mutate-context! k)
                         (counter)
                         (clicks)
                         (str "consuming " k ": [" v "]")])]
    [:div.pa3

     (counter)
     (clicks)

     (v/provide {:a (:a @state)
                 :b (:b @state)
                 :c (:c @state)}
                (class/consume [a :a]
                                [:<>
                                 (show-consumer :a a)
                                 (class/consume [b :b]
                                                 [:<>
                                                  (show-consumer :b b)
                                                  (class/consume [c :c]
                                                                  (show-consumer :c c))])]))]))