(ns chia.view.state-test
  (:require
   [cljs.test :refer [deftest is are testing]]
   [chia.triple-db :as d]
   [chia.view :as v]
   [chia.view.legacy :as vl]
   [chia.view.util :as vu]))

(defn get-el []
  (vu/find-or-append-element "chia-view-state-test" "div"))

(deftest local-state
  (let [el (get-el)
        render #(v/render-to-dom % el)]
    (testing "atom from initial-state"
      (let [log (atom [])
            local-state (atom nil)
            view (vl/view x
                   {:view/initial-state 0
                    :view/did-mount #(reset! local-state (:view/state %))}
                   [{:keys [view/state] :as this}]
                   (swap! log conj @state)
                   [:div "hello"])]

        (render (view))
        (is (= @log [0]))
        (swap! @local-state inc)
        (v/flush!)

        (is (= @log [0 1]))
        (reset! @local-state "x")
        (v/flush!)
        (is (= @log [0 1 "x"]))))))

(deftest triple-db

  (testing "React to global state (chia.triple-db)"

    (d/transact! [{:db/id 1
                   :name "Herbert"
                   :occupation "Chimney Sweep"}])

    (let [log (atom [])
          el (get-el)
          view (vl/view x [{:keys [db/id]}]
                 (swap! log conj (d/get id :name))
                 [:div "hello"])
          render #(v/render-to-dom (view {:db/id %}) el)]

      (render 1)
      (is (= 1 (count @log)))

      (d/transact! [[:db/add 1 :name "Frank"]])
      (v/flush!)

      (is (= 2 (count @log)))
      (is (= "Frank" (last @log)))

      (d/transact! [{:db/id 2 :name "Gertrude"}])
      (v/flush!)

      (is (= 2 (count @log)))

      (render 2)

      (is (= 3 (count @log)))
      (is (= "Gertrude" (last @log))))))

