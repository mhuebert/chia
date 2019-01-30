(ns chia.jss
  (:require ["jss" :as jss]
            ["reset-jss" :as reset-jss]
            ["jss-preset-default" :default jss-preset]
            [chia.util.js-interop :as j]
            [chia.view.util :as vu]))


(def JSS
  (memoize
   (fn
     ([]
      (JSS (cond-> (jss-preset)
                   (exists? js/window)
                   (j/assoc! :insertionPoint (vu/find-or-append-element "chia-jss")))))
     ([presets]
      (jss/create presets)))))

(defonce global-reset!
         (memoize
          (fn []
            (-> ^js (JSS)
                (.createStyleSheet reset-jss)
                (.attach)))))

(defonce ^js page-styles
         (memoize
          (fn []
            (when (exists? js/window)
              (-> (JSS)
                  (j/call :createStyleSheet #js {})
                  (j/call :attach))))))

(def classes!
  (memoize
   (fn [styles]
     (-> ^js (JSS)
         (.createStyleSheet (clj->js styles))
         (.attach)
         (j/get :classes)
         (js->clj :keywordize-keys true)))))

(def class!
  (memoize
   (fn [styles]
     (-> (classes! {:inline styles})
         :inline)
     #_(let [^js rule (.addRule (page-styles) (clj->js styles))]
         (.-className rule)))))

(defn to-string [styles]
  (-> ^js (JSS)
      (.createStyleSheet (clj->js styles))
      (str)))
