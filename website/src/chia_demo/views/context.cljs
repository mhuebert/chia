(ns chia-demo.views.context
  (:require [chia.view :as v]
            [chia.view.legacy :as legacy]
            [chia.reactive :as r]
            [chia.view.registry :as registry]))

(defn counter []
  (let [{:keys [view/state]} registry/*view*]
    (r/silently
     (swap! state update :count (fnil inc 0)))
    [:div (str "Renders:" (:count @state 0))]))

(defn clicks []
  (let [{:keys [view/state]} registry/*view*]
    [:a.db {:on-click #(swap! state update :clicks (fnil inc 0))}
     (str "Clicks:" (:clicks @state 0))]))

(legacy/defview demo
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

              (hooks/provide {:a (:a @state)
                          :b (:b @state)
                          :c (:c @state)}
                         (legacy/consume [a :a]
                                    [:<>
                                     (show-consumer :a a)
                                     (legacy/consume [b :b]
                                                [:<>
                                                 (show-consumer :b b)
                                                 (legacy/consume [c :c]
                                                            (show-consumer :c c))])]))]))