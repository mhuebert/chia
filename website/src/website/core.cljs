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

   #_[website.views.components :as examples]
            [website.util :as u]

            [clojure.string :as string]

            [goog.events :as events]
            [goog.functions :as gf]))

(def header-github-url "https://www.github.com/mhuebert/chia")

(def main-nav-items (list ["Home" "/"]
                          ["Docs" "/docs/"]

                          ;; TODO
                          ;; re-implement `components` with chia/material
                          #_["Components" "/components"]

                          ;; TODO
                          ;; update /code with chia references
                          #_["Code" "/code"]))


(enable-console-print!)
(let [display-mobile? #(<= (.-innerWidth js/window) 500)
      mobile? (display-mobile?)]
  (events/listen js/window "resize" (gf/throttle #(let [mobile? (display-mobile?)]
                                                    (d/transact! [{:db/id :ui/globals
                                                                   :media/mobile? mobile?
                                                                   :layout/drawer-open? (not mobile?)}])) 300))
  (d/transact! [{:db/id :ui/globals
                 :media/mobile? mobile?
                 :theme/dark? false
                 :layout/drawer-open? (not mobile?)}]))

(v/defclass layout [this
                 main-content]
  (let [{:keys [theme/dark?
                media/mobile?
                layout/drawer-open?]} (d/entity :ui/globals)
        home? (= (d/get :router/location :segments) [])]
    [:.mdc-typography
     (layout/page-meta)
     (for [[label href] main-nav-items]
       [:a.dib.pa2.mh2.no-underline.relative.color-inherit
        {:href href
         :class (when (layout/active? href) "mdc-toolbar--active-link")} label])
     [:.ph4-ns.ph3.pv1.relative
      main-content]


     ;; TODO
     ;; re-implement below content in 'new' material-ui
     #_(ui/ToolbarWithContent
        {:classes ["z-3"
                   (when dark? "mdc-theme--dark")]
         :style {:background-color (if dark? "#464646" "#eaeaea")
                 :color "inherit"}
         :waterfall true
         :fixed true}
        (ui/ToolbarRow
         {:class "ph4-ns ph3"}
         (ui/ToolbarSection
          {:classes ["flex items-center mw-page center"]}
          (for [[label href] main-nav-items]
            [:a.pv2.mh2.no-underline.relative.color-inherit
             {:href href
              :class (when (layout/active? href) "mdc-toolbar--active-link")} label])

          [:.flex-auto]
          ((if mobile? ui/Switch ui/SwitchField)
           (cond-> {:id "theme"
                    :color (when dark? :accent)
                    :checked dark?
                    :on-change #(d/transact! [[:db/update-attr :ui/globals :theme/dark? not]])}
                   (not mobile?) (assoc :label "Dark theme"
                                        :field-classes ["ph2"])))))

        [:.ph4-ns.ph3.pv1.relative
         main-content])]))

(v/defclass root
  "The root component reads current router location from re-db,
   and will therefore re-render whenever this value changes."
  []
  (let [segments (d/get :router/location :segments)]
    (match segments
           [] [layout [:div
                       [:.serif.tc.mw-page.center.mv3
                        [:.f0.pt4 "Chia"]
                        [:.f4 "Simple React apps in ClojureScript."]]
                       (code/repo-file-page {:toc? false} "docs" "intro.md")]]
           #_#_["components"] (layout (examples/library {}))
           #_#_["components" id] (layout (examples/library {:detail-view id}))
           ["docs"] [layout (docs/doc-page "/")]
           ["docs" & doc-path] [layout (docs/doc-page (string/join "/" doc-path))]
           ["code"] [layout (code/repositories-index)]
           ["code" repo] [layout (code/repository-page repo)]
           ["code" repo file] [layout (views/page (code/repo-file-page repo file))]

           :else [:div "not found"])))

(defn ^:dev/after-load render []
  (v/render-to-dom (root) "website"))

(defn ^:export init []
  (r/listen
   (fn [route]
     (prn :route route)
     (d/transact! [(assoc route :db/id :router/location)])))
  (render))
