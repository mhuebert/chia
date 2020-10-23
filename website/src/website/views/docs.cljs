(ns website.views.docs
  (:require [chia.view.class :as vl :refer [defclass]]
            [website.views.markdown :refer [md]]
            [goog.string.path :as path]
            [clojure.string :as string]
            [website.util :as util]
            [website.views :as views]
            [chia.view :as v]))

(def download-root "https://braintripping.github.io/re-view")
(def doc-edit-root "https://github.com/braintripping/re-view/edit/master/docs")

(defn hyphens->readable [s]
  (if (= s "re-view")
    "Re-View"
    (string/replace s #"(?:^|-)([a-z])" (fn [[_ s]] (str " " (string/upper-case s))))))



#_(defn doc-breadcrumb [url]
    (let [all-segments (routing/segments url)]
      (let [num-segments (count all-segments)]
        (->> all-segments
             (map-indexed
               (fn [i label]
                 [(hyphens->readable label)
                  (as-> (take (inc i) all-segments) segments
                        (string/join "/" segments)
                        (str "/docs/" segments)
                        (if-not (= i (dec num-segments)) (str segments "/") segments))]))
             (cons ["Docs" "/docs/"])
             (breadcrumb)))))

(def index nil)

(defn path->keys [url]
  (if (= url "/")
    (list "index")
    (-> (string/replace url #"\.md$" "")
        (string/split #"/"))))

(def get-index (fn [cb]
                 (util/GET :json "https://braintripping.github.io/re-view/json/index.json"
                           (fn [{:keys [value]}]
                             (set! index
                                   (->> (js->clj value :keywordize-keys true)
                                        (reduce (fn [m {:keys [title name path] :as doc}]
                                                  (assoc-in m (concat (path->keys path) (list :*file)) doc)) {})))
                             (cb)))))

(defclass doc-page
  {:view/did-mount (fn [{:keys [view/state
                                edit-url] :as this} url]
                     (when-not index (get-index #(v/force-update this))))}
  [this url]
  (let [path-ks (path->keys url)
        {{file-path :path} :*file
         :as               current-index} (get-in index (path->keys url))]
    (cond
      (nil? index) [:div "Loading..."]
      file-path (views/markdown-page {:read (path/join download-root file-path)
                                      :edit (path/join doc-edit-root file-path)})
      current-index (views/index-page current-index))))

(def sidebar
  [{:text-primary "Getting Started"
    :href         "/docs/re-view/getting-started"}])
