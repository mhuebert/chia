(ns chia.material-ui
  (:require [chia.view :as v]
            [chia.view.util]
            [clojure.string :as str]
            [chia.view.hiccup :as hiccup]
            [chia.view.props :as props]
            ["@material-ui/core/IconButton" :default IconButton*])
  (:require-macros [chia.material-ui :as m]))

(defn to-js
  ([props] (to-js {} props))
  ([options props]
   (if (object? props)
     props
     (props/adapt-props (-> options
                            (update :updates (partial merge-with into) {clj->js [:classes]})
                            (update :lift-nses (fnil conj #{}) "material")) props))))

(defn wrap-component [component options]
  (fn [& args]
    (let [props (hiccup/get-props args 0)
          props? (hiccup/props? props)]
      (hiccup/make-element component
                           (to-js options (if props? props {}))
                           args
                           (if props? 1 0)))))

(def icon-adjustments
  {:add  #(assoc % :font-weight "bold")
   :edit #(update % :font-size * 0.7)})

(defn icon
  ([icon-name] (icon {} icon-name))
  ([{:keys [size style]
     :or   {size :m}
     :as   props} icon-name]
   (let [adjustments (icon-adjustments icon-name)]
     [:div.material-icons
      (-> props
          (dissoc :size)
          (update :class str
                  " "
                  (when (:on-click props) "pointer "))
          (v/merge-props (cond-> {}
                                 (:on-click props) (assoc :class "pointer")
                                 (not (:font-size style)) (assoc-in [:style :font-size] (case size :s 18
                                                                                                   :m 24
                                                                                                   :l 36
                                                                                                   :xl 48))))
          (cond-> adjustments (update :style adjustments)))
      (-> icon-name
          (name)
          (str/replace "-" "_"))])))

(m/defm icon-button*)

(defn icon-button
  ([icon-name]
   (icon-button {} icon-name))
  ([{:as        props
     icon-props :icon} icon-name]
   (icon-button* (-> (dissoc props :icon)
                     (assoc :key icon-name))
                 (icon icon-props icon-name))))

