(ns chia.material-ui
  (:require  [chia.view :as v]
             ["@material-ui/core/IconButton" :default IconButton*]
             ["@material-ui/core/styles" :refer [withStyles createMuiTheme MuiThemeProvider]])
  (:require-macros [chia.material-ui :as m]))

(defn icon
      ([icon-name] (icon {} icon-name))
      ([attrs icon-name]
       [:div.material-icons (cond-> (assoc attrs :key icon-name)
                                    (:on-click attrs) (assoc-in [:style :cursor] "pointer")) (name icon-name)]))

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