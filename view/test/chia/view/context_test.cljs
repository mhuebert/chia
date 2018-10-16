(ns chia.view.context-test
  (:require [chia.view :as v]
            [chia.view.util :as vu]
            [chia.view.context :as c]
            [cljs.test :as test :refer [is]]
            [chia.reactive.atom :as ra]
            [chia.reactive :as r]))

(defn render! [view]
  (v/render-to-dom (view) (vu/find-or-append-element :context-test :div))
  (v/flush!))

(def last-result
  (atom {}))

(defn record! [& keyvals]
  (apply swap! last-result assoc keyvals)
  nil)

(defn results [& ks]
  ((apply juxt ks) @last-result))

(test/deftest view-context

  (is (do (render! (v/view x []
                     (c/provide {::first-name "Herman"
                                 ::last-name "Früling"}
                       (c/consume [first-name ::first-name
                                   last-name ::last-name]
                         (record! :F first-name
                                  :L last-name)))))
          (= (results :F :L)
             ["Herman" "Früling"]))
      "Contexts are propagated correctly")

  (test/testing "Context reactivity"
    (let [state (atom {})
          helper (fn [k]
                   (swap! state update-in [k :count] inc)
                   (ra/get-in state [k :trigger])
                   nil)
          get-count (fn [k]
                      (get-in @state [k :count] 0))
          trigger! (fn [k]
                     (ra/update-in! state [k :trigger] inc)
                     (v/flush!))
          count-outer-inner #(mapv get-count [:outer :inner])]
      (render! (v/view x []
                 (c/provide {:x "X"
                             :y "Y"}
                   (c/consume [x :x]
                     [:div
                      (helper :outer)
                      (c/consume [y :y]
                        (helper :inner))]))))

      (is (= (count-outer-inner) [1 1])
          "Basic render-count works")

      (trigger! :outer)

      (is (= (count-outer-inner) [2 2])
          "Outer context re-renders itself + inner")

      (trigger! :inner)

      (is (= (count-outer-inner) [2 3])
          "Inner context re-renders only itself (it is an independent reactivity context)"))))
