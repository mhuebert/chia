(ns website.views.material
  (:require ["@material-ui/core/AppBar" :default AppBar]
            ["@material-ui/core/IconButton" :default IconButton]
            ["@material-ui/core/Toolbar" :default Toolbar]
            [chia.material-ui :as m]))

(m/defm app-bar)
(m/defm toolbar)
(m/defm icon-button)