(ns chia.static.page
  (:require [hiccup.page :as page]
            [chia.static.assets :as assets]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HTML generation

(defn element-tag [kw-or-hiccup]
  (cond-> kw-or-hiccup
          (keyword? kw-or-hiccup) (vector)))

(defn script-tag [str-or-map]
  [:script
   (if (string? str-or-map)
     str-or-map
     (update str-or-map :src assets/asset-path))])

(defn meta-tag [k v]
  [:meta {(if (some #{"Expires"
                      "Pragma"
                      "Cache-Control"
                      "imagetoolbar"
                      "x-dns-prefetch-control"} (name k))
            :http-equiv
            :name) (name k)
          :content v}])

(defn style-tag [str-or-map]
  (if (string? str-or-map)
    [:style str-or-map]
    [:link (-> str-or-map
               (assoc :rel "stylesheet")
               (update :href assets/asset-path))]))

(defn html-page
  "Return HTML string for title and props"
  ([title page-props]
   (html-page (assoc page-props :title title)))
  ([{:as page-props
     :keys [lang
            title
            charset
            styles
            meta
            head
            body]
     body-scripts :scripts/body
     head-scripts :scripts/head
     :or {lang "en"
          charset "UTF-8"}}]
   (page/html5 {:lang lang}

               [:head

                (for [[k v] meta]
                  (meta-tag k v))

                [:meta {:http-equiv "Content-Type"
                        :content (str "text/html; charset=" charset)}]

                (when title
                  [:title title])

                (map style-tag styles)
                (map element-tag head)
                (map script-tag head-scripts)]

               [:body
                (map element-tag body)
                (map script-tag body-scripts)])))

(comment
 (html-page "Welcome"
            {:styles [{:href "/some/styles.css"}
                      ".black {color: #000}"]
             :body [:div#app]
             :scripts/body [{:src "/some/script.js"}
                            "alert('hi!')"]}))