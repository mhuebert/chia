(ns chia.material-ui
  (:require [chia.view :as v]
            [clojure.string :as str]
            ["@material-ui/core/IconButton" :default IconButton*]
            ["@material-ui/core/styles" :refer [withStyles createMuiTheme MuiThemeProvider]])
  (:require-macros [chia.material-ui :as m]))

(defn icon
  ([icon-name] (icon {} icon-name))
  ([{:keys [size]
     :or {size :m}
     :as attrs} icon-name]
   [:div.material-icons
    (-> attrs
        (dissoc :size)
        (update :class str
                " "
                (when (:on-click attrs) "cursor-pointer ")
                (case size :s "md-18"
                           :m "md-24"
                           :l "md-36"
                           :xl "md-48"
                           nil)))
    (-> icon-name
        (name)
        (str/replace "-" "_"))]))

(m/defm icon-button*)

(defn icon-button
  ([icon-name] (icon-button {} icon-name))
  ([attrs icon-name]
   [icon-button* (-> attrs
                     (assoc :key icon-name)
                     (dissoc :icon))
    (icon (get attrs :icon) icon-name)]))

(defn create-theme
  ([] (create-theme {}))
  ([options]
   (createMuiTheme (clj->js options))))

(def theme-provider (v/adapt-react-class MuiThemeProvider))



