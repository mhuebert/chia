(ns chia.view.props
  (:require [chia.util :as u]
            [chia.view.hiccup.impl :as hiccup-impl]
            [chia.view.render-loop :as render-loop]
            [chia.view.hiccup :as hiccup]))

(defn- update-change-prop [props]
  (cond-> props
          (contains? props :on-change) (update :on-change render-loop/apply-sync!)))

(defn wrap-props
  "Wraps :on-change handlers of text inputs to apply changes synchronously."
  [props tag]
  (cond-> props
          (and ^boolean (or (identical? "input" tag)
                            (identical? "textarea" tag))) update-change-prop))

(defn to-element [x]
  (hiccup/element {:wrap-props wrap-props} x))

(defn adapt-props
  [{:keys [->element-keys
           ->js-keys
           lift-nses
           wrap-props]} props]
  (-> props
      (cond-> lift-nses (u/lift-nses lift-nses)
              ->element-keys (u/update-some-keys ->element-keys to-element)
              ->js-keys (u/update-some-keys ->js-keys clj->js)
              wrap-props (wrap-props))
      (update-change-prop)
      (hiccup-impl/props->js)))

(defn merge-props
  [m1 m2]
  (merge m1
         m2
         (merge-with #(str %1 " " %2)
                     (select-keys m1 [:class])
                     (select-keys m2 [:class]))
         (merge-with merge
                     (select-keys m1 [:style])
                     (select-keys m2 [:style]))))

(defn partial-props [view initial-props]
  (fn [props & children]
    (let [[props children] (if (or (map? props)
                                   (nil? props)) [props children]
                                                 [{} (cons props children)])]
      (into [view (merge-props initial-props props)] children))))