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
     :as props} icon-name]
   [:div.material-icons
    (-> props
        (dissoc :size)
        (update :class str
                " "
                (when (:on-click props) "cursor-pointer ")
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
  ([icon-name]
   (icon-button {} icon-name))
  ([{:as props
     icon-props :icon} icon-name]
   (icon-button* (-> (dissoc props :icon)
                     (assoc :key icon-name))
                 (icon icon-props icon-name))))

(defn create-theme
  ([] (create-theme {}))
  ([options]
   (createMuiTheme (clj->js options))))

(def theme-provider (v/adapt-react-class MuiThemeProvider))
