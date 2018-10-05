(ns chia-demo.views.context
  (:require [chia.view :as v]
            [chia.view.context :as c]))

(defn counter []
  (let [{:keys [view/state]} v/*current-view*]
    (v/swap-silently! state update :count (fnil inc 0))
    [:div (str "Renders:" (:count @state 0))]))

(defn clicks []
  (let [{:keys [view/state]} v/*current-view*]
    [:a.db {:on-click #(swap! state update :clicks (fnil inc 0))}
     (str "Clicks:" (:clicks @state 0))]))

(v/defview demo
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

              (c/provide {:a (:a @state)
                          :b (:b @state)
                          :c (:c @state)}
                         (c/consume [a :a]
                                    [:<>
                                     (show-consumer :a a)
                                     (c/consume [b :b]
                                                [:<>
                                                 (show-consumer :b b)
                                                 (c/consume [c :c]
                                                            (show-consumer :c c))])]))]))