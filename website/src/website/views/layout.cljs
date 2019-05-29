(ns website.views.layout
  (:require [chia.view.legacy :as v :refer [defclass]]
            [chia.db :as d]
            [clojure.string :as string]
            [website.util :as util]))

(defn page-meta []
  (list
    ;; sets properties on document
    (util/sync-element-css!
      {:classes     (if (d/get :ui/globals :theme/dark?)
                      ["mdc-theme--dark" "bg-mid-gray"]
                      ["bg-near-white"])
       :style       {:min-height "100%"}
       :get-element #(when (exists? js/document)
                       (.-documentElement js/document))})

    ;; stylesheets for code
    [:link {:rel  "stylesheet"
            :type "text/css"
            :href (if (d/get :ui/globals :theme/dark?) "/styles/railscasts.css"
                                                       "/styles/github.css")}]))

(defn active? [href]
  (let [path (d/get :router/location :path)]
    (case href "/" (= path href)
               (string/starts-with? path href))))

