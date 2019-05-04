(ns chia-demo.views.reactive-triple
  (:require [chia.view :as v]
            [chia.reactive.atom-db :as db]
            [chia.db :as t]
            [chia-demo.views.util :as u]
            [cljs.pprint :as pp]
            [applied-science.js-interop :as j]))

(def last-color (atom 0))

(defn add-color! [parent]
  (let [child-id (swap! last-color inc)]
    (t/transact! [[:db/update-attr parent :children (fnil conj []) child-id]
                  {:db/id child-id
                   :color (rand-nth u/color-names)}])))

(v/defclass color-box
  {:key :id
   :props/consumed #{:path}}
  [{:keys [id
           view/state] :as this} parent-angle]
  [:div.bg-darken-1.b--darken-4.ma1.flex.flex-row.items-center.justify-center.flex-wrap.relative.br3.overflow-hidden.ba
   {:style {:background-color (t/get id :color)
            :margin 10
            :min-width 50
            :min-height 50}}
   (u/show-count this)

   [:div {:on-click #(do (.preventDefault %)
                         (t/transact! [[:db/add id :color (rand-nth u/color-names)]]))}
    (u/icon :color-lens)]

   (u/icon {:style {:transform (str "rotate(" (+ (or parent-angle 0)
                                                 (:angle @state 0)) "deg)")
                    :transition "transform 0.3s ease-out"}
            :on-click #(swap! state update :angle + 80)
            :class "pointer"}
           :rotate-right)

   (for [child-id (t/get id :children)]
     (color-box {:id child-id}
                (-> (:angle @state 0)
                    (+ parent-angle))))

   [:div {:on-click #(do (.preventDefault %)
                         (add-color! id))}
    (u/icon :add-circle)]])

(v/defclass demo
  {:view/initial-state {:renders 1}
   :demo/title "Triple-DB Demo"
   ;:view/will-unmount #(db/assoc! ::colors {})
   :view/did-mount #(t/transact! [{:db/id 0
                                   :color (rand-nth u/color-names)}])}
  [this]
  (list [:div.ba.b--darken-2
         [:div.flex.flex-row.flex-wrap.items-center
          (color-box {:id 0} 0)
          [:div {:on-click #(add-color! [::colors])}
           (u/icon :add-circle)]]]
        [:div.text-pre-wrap.ma3.pos-relative
         (u/show-count this)
         [:br]
         (with-out-str (pp/pprint (db/deref)))]))