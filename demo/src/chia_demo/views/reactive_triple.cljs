(ns chia-demo.views.reactive-triple
  (:require [chia.view :as v]
            [chia.reactive.atom-db :as db]
            [chia.triple-db :as t]
            [chia-demo.views.util :as u]
            [cljs.pprint :as pp]
            [chia.util.js-interop :as j]))

(def last-color (atom 0))

(defn add-color! [parent]
  (let [child-id (swap! last-color inc)]
    (t/transact! [[:db/update-attr parent :children (fnil conj []) child-id]
                  {:db/id child-id
                   :color (rand-nth u/color-names)}])))

(v/defview color-box
  {:key :id
   :props/consumed #{:path}}
  [{:keys [id
           view/state
           view/classes] :as this} parent-angle]
  [:div.bg-darken-1.b-darken-4.margin-1.flex.flex-row.items-center.justify-center.flex-wrap.relative.round-4.overflow-hidden.b-solid.b-1
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
            :class "cursor-pointer"}
           :rotate-right)

   (v/for [child-id (t/get id :children)]
     (color-box {:id child-id}
                (-> (:angle @state 0)
                    (+ parent-angle))))

   [:div {:on-click #(do (.preventDefault %)
                         (add-color! id))}
    (u/icon :add-circle)]])

(v/defview demo
  {:view/initial-state {:renders 1}
   :demo/title "Triple-DB Demo"
   ;:view/will-unmount #(db/assoc! ::colors {})
   :view/did-mount #(t/transact! [{:db/id 0
                                   :color (rand-nth u/color-names)}])}
  [this]
  (list [:div.b-1.b-darken-2
         [:div.flex.flex-row.flex-wrap.items-center
          (color-box {:id 0} 0)
          [:div {:on-click #(add-color! [::colors])}
           (u/icon :add-circle)]]]
        [:div.text-pre-wrap.margin-all-4.pos-relative
         (u/show-count this)
         [:br]
         (with-out-str (pp/pprint (db/deref)))]))