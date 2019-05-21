(ns website.static
  (:require [cljs-static.page :as page]))

(defn index []
  (page/html-page
    {:title "Chia: User Interfaces with ClojureScript and React"
     :meta {:viewport "width=device-width, initial-scale=1"}
     :styles [{:href "https://fonts.googleapis.com/css?family=IBM+Plex+Mono:400,400i|IBM+Plex+Sans:400,600|Material+Icons"}
              {:href "https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css"}
              {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"}
              {:href "/styles/website.css"}]
     :body [:div#website]
     :scripts/body [{:src "/js/website.js"}]}))