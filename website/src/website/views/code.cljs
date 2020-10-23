(ns website.views.code
  (:require [chia.view :as v]
            [chia.view.class :as legacy]
            [website.views :as views]
            [goog.string.path :as path]
            [chia.view.props :as props]))


(legacy/defclass repo-file-page
  [this category file-path]
  (views/markdown-page (merge {:read (-> (str "/docs/" (munge category))
                                         (path/join file-path))
                               :edit (-> (str "https://github.com/mhuebert/chia/edit/master/website/docs/" (munge category))
                                         (path/join file-path))}
                              (:view/props this))))


(legacy/defclass repository-row
  {:key (fn [_ owner repo] (str owner repo))}
  [_ owner repo]
  [:.f6.flex.items-center
   [:a.mr2 {:href (str "https://www.github.com/mhuebert/chia/tree/master/" (munge repo))} "source"]
   [:a.mr2 {:href (str "/code/" repo "/CHANGELOG.md")} "changelog"]
   (views/clickable-version owner repo)])

(defn repository-page [repo]
  (views/page nil [:.pv3 (repository-row repo)]))

(legacy/defclass repositories-index []

  (views/page nil
              [:div.pb3

               [:.f4.o-50.mt4 "Core"]

               [:.f5.b.mt3 "Re-View"]
               (repository-row "re-view" "re-view")

               [:.f4.o-50.mt4 "Components"]

               [:.f5.b.mt3 "Material Design Components"]
               (repository-row "re-view" "material")

               [:.f5.b.mt3 "Rich Text Components"]
               (repository-row "re-view" "prosemirror")

               [:.f4.o-50.mt4 "Dependencies"]

               [:.f5.b.mt3 "Re-DB"]
               (repository-row "re-db" "re-db")]))

