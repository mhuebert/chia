(ns website.static
  (:require [static.page :as page]
            [hiccup.core :as hiccup]))

(defn index []
      (hiccup/html
       (page/html
        {:title        "Chia: User Interfaces with ClojureScript and React"
         :meta         {:viewport "width=device-width, initial-scale=1"}
         :styles       [{:href "https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css"}
                        {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"}
                        {:href "/styles/website.css"}]
         :body         [:div#website]
         :scripts/body [{:src "/js/website.js"}]})))