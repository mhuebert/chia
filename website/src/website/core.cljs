(ns website.core
  (:require [cljs.core.match :refer-macros [match]]

            [chia.view :as v]
            [chia.routing :as r]
            [chia.db :as d]

            [website.views :as views]
            [website.views.docs :as docs]
            [website.views.code :as code]
            [website.views.markdown :refer [md]]
            [website.views.layout :as layout]

            ["@material-ui/core/useScrollTrigger" :default use-scroll-trigger]
            ["@material-ui/core/Container" :default container]

            [website.views.material :as m]

            [clojure.string :as string]

            [goog.events :as events]
            [goog.functions :as gf]))

(def header-github-url "https://www.github.com/mhuebert/chia")

(def main-nav-items (list ["Home" "/"]
                          ["Docs" "/docs/"]
                          #_["Components" "/components"]
                          #_["Code" "/code"]))

(v/defn layout [main-content]
  (let [{:keys [theme/dark?
                media/mobile?
                layout/drawer-open?]} (d/entity :ui/globals)
        home? (= (d/get :router/location :segments) [])
        trigger (use-scroll-trigger #js{:disableHysteresis true
                                        :threshold         0})]
    [:.sans-serif
     (layout/page-meta)
     [m/app-bar {:position "sticky"
                 :elevation (if trigger 4 0)}
      [m/toolbar
       {:style     {:background-color "rgb(234, 234, 234)"}
        :classes {:root "black"}}
       [container {:max-width "sm"}
        [:div.nl2.nr2
         (for [[label href] main-nav-items]
           [:a.dib.mh2.no-underline.relative.color-inherit
            {:href  href
             :class (when (layout/active? href) "mdc-toolbar--active-link")} label])]
        #_((if mobile? ui/Switch ui/SwitchField)
         (cond-> {:id        "theme"
                  :color     (when dark? :accent)
                  :checked   dark?
                  :on-change #(d/transact! [[:db/update-attr :ui/globals :theme/dark? not]])}
                 (not mobile?) (assoc :label "Dark theme"
                                      :field-classes ["ph2"])))]]]
     [container {:max-width "sm"}
      main-content]]))

(v/defn root
  "The root component reads current router location from re-db,
   and will therefore re-render whenever this value changes."
  []
  (let [segments (d/get :router/location :segments)]
    (match segments
           [] [layout [:div
                       [:.serif.tc.mv3
                        [:.pt4 {:style {:font-size "4rem"}} "Chia"]
                        [:.f4 "Simple React apps in ClojureScript."]]
                       (code/repo-file-page {:toc? false} nil "intro.md")]]
           #_#_["components"] (layout (examples/library {}))
           #_#_["components" id] (layout (examples/library {:detail-view id}))
           ["docs"] [layout (docs/doc-page "/")]
           ["docs" & doc-path] [layout (docs/doc-page (string/join "/" doc-path))]
           ["code"] [layout (code/repositories-index)]
           ["code" repo] [layout (code/repository-page repo)]
           ["code" repo file] [layout (views/page (code/repo-file-page repo file))]

           :else [:div "not found"])))

(defn ^:dev/after-load render []
  (v/render-to-dom (root) "website" {:reload? true}))

(defn init-ui []
  (let [on-resize #(let [mobile? (<= (.-innerWidth js/window) 500)]
                     (d/transact! [{:db/id               :ui/globals
                                    :media/mobile?       mobile?
                                    :layout/drawer-open? (not mobile?)}]))]
    (events/listen js/window "resize" (gf/throttle on-resize 300))
    (on-resize)
    (d/transact! [[:db/add :ui/globals :theme/dark? false]])))

(defn init []
  (init-ui)
  (r/listen
    (fn [route]
      (d/transact! [(assoc route :db/id :router/location)])))
  (render))
