(ns chia-demo.app
  (:require [chia.view :as v]
            [chia.jss :as jss]
            [chia-demo.styles]

            [chia-demo.views.components :as c]
            [chia-demo.views.reactive :as atom-db-demo]
            [chia-demo.views.reactive-triple :as triple-db-demo]

            [chia-demo.views.util :as u]
            [chia.reactive.atom-db :as db]))

(jss/global-reset!)

(def sections [triple-db-demo/demo
               atom-db-demo/demo
               c/components])

(v/defview show-section
  {:key (fn [_ handler] (v/class-get handler :demo/title))}
  [{:keys [view/classes]} handler & args]
  (let [label (v/class-get handler :demo/title)
        expanded? (db/get-in [:section label :expanded?] true)]
    (cond-> [:div.pad-v-2.bg-darken-2.hover-bg-darken-3.cursor-pointer.flex.flex-row.items-center.b-2.b-top.b-darken-4
             {:on-click #(db/assoc-in! [:section label :expanded?] (not expanded?))}
             (u/icon [] (if expanded? :expand-less :expand-more))
             label]
            expanded?
            (cons
             (list [:div.pad-2 (handler)])))))

(v/defview layout []
  (->> sections
       (map show-section sections)))

(defn ^:dev/after-load ^:export render []
  (v/render-to-dom (layout {}) "app"))

(defn init-worker []
  (let [path "/worker.js"]
    (.. js/navigator
        -serviceWorker
        (register path)
        (then
         (fn [registration]
           (prn "SW registered with scope:" (.-scope registration)))
         (fn [err]
           (prn "SW failed:" err))))
    #_(let [worker (js/Worker. path)]
        (doto worker
          (.addEventListener "message" js/console.log)
          (.postMessage "hello world")))))

#_(when (exists? js/navigator.serviceWorker)
    (init-worker))

(comment
 (render))