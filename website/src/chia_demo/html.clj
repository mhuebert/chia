(ns chia-demo.html
  (:require [static.page :as page]
            [hiccup.core :as hiccup]))

(defn index []
      (hiccup/html
       (page/html
        {:title        "Chia Example"
         :meta         {:viewport "width=device-width, initial-scale=1"}
         :styles       [{:href "https://fonts.googleapis.com/css?family=IBM+Plex+Mono:400,400i|IBM+Plex+Sans:400,600|Material+Icons"}
                        {:href "/css/tachyons.min.css"}]
         :body         [:div#app]
         :scripts/body [{:src "/compiled/website.js"}
                        "chia_demo.website.render()"]})))