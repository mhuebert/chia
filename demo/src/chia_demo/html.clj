(ns chia-demo.html
  (:require [chia.static.page :as page]))

(defn index []
  (page/html-page
    {:title "Chia Example"
     :meta {:viewport "width=device-width, initial-scale=1"}
     :styles [{:href "https://fonts.googleapis.com/css?family=IBM+Plex+Mono:400,400i|IBM+Plex+Sans:400,600|Material+Icons"}
              {:href "/tachyons.min.css"}]
     :body [:div#app]
     :scripts/body [{:src "/compiled/app.js"}
                    "chia_demo.app.render()"]}))