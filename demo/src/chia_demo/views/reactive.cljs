(ns chia-demo.views.reactive
  (:require [chia.view :as v]
            [chia.reactive.atom-db :as db]
            [chia-demo.views.util :as u]
            [cljs.pprint :as pp]
            [chia-demo.styles :as styles]))

(defn add-color! [path]
  (db/update-in! path
                 (fn [m]
                   (assoc-in m [(count m) :color] (rand-nth u/color-names)))))

;; re: css
;; - using a design system: consistent spacing, typography, colors
;; - toggling states dynamically
;;
;;
;; ideas
;; - (css/class {:border [1 :solid (:black css/colors)]})
;;
;; (def styles {:b-on {...}})
;;
;; ...(css/class (:b-on styles))
;;
;; css/class => idempotent, returns a unique class-name for the provided map, compiles & injects into page.
;;
;;
;; - _env_ - eg. read react context? somehow support theming.


(v/defview color-box
  {:key :path
   :props/consumed #{:path}}
  [{:keys [path
           view/state
           view/classes] :as this} parent-angle]
  [:div.bg-darken-1.b--darken-4.ma1.flex.flex-row.items-center.justify-center.flex-wrap.relative.br3.overflow-hidden.ba.bw1.ma2
   {:style {:background-color (db/get-in (conj path :color) "#fff")
            :min-width 50
            :min-height 50}}
   (u/show-count this)

   [:div {:on-click #(do (.preventDefault %)
                         (db/assoc-in! (conj path :color) (rand-nth u/color-names)))}
    (u/icon :color-lens)]

   (u/icon {:style {:transform (str "rotate(" (+ (or parent-angle 0)
                                                 (:angle @state 0)) "deg)")
                    :transition "transform 0.3s ease-out"}
            :on-click #(swap! state update :angle + 80)
            :class "pointer"}
           :rotate-right)

   (for [k (db/keys-in path)]
     (when (not= :color k)
       (color-box {:path (conj path k)} (+ (:angle @state 0)
                                           (or parent-angle 0)))))

   [:div {:on-click #(do (.preventDefault %)
                         (add-color! path))}
    (u/icon :add-circle)]])

(v/defview demo
  {:view/initial-state {:renders 1}
   :demo/title "Reactive Atom Demo"
   :view/will-unmount #(db/assoc! ::colors {})
   :view/did-mount #(add-color! [::colors])}
  [this]
  (list [:div.ba.b--darken-2
         [:div.flex.flex-row.flex-wrap.items-center
          (for [n (db/keys-in [::colors])
                :when (not= :color n)]
            (color-box {:path [::colors n]}))
          [:div {:on-click #(add-color! [::colors])}
           (u/icon :add-circle)]]]
        [:div.pre-wrap.ma3.relative
         (u/show-count this)
         [:br]
         (with-out-str (pp/pprint (db/deref)))]))