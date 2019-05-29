(ns chia.jss
  (:require ["jss" :as jss]
            ["reset-jss" :as reset-jss]
            ["jss-preset-default" :default jss-preset]
            [applied-science.js-interop :as j]
            [chia.view.util :as vu]
            [chia.util :as u]))

(defonce JSS
  (memoize
    (fn
      ([]
       (JSS (cond-> (jss-preset)
                    (exists? js/window)
                    (j/assoc! :insertionPoint (vu/find-or-append-element "chia-jss")))))
      ([presets]
       (jss/create presets)))))

(defonce Stylesheet
  (memoize
    (fn [key]
      (.createStyleSheet ^js (jss)))))

(defonce global-reset!
  (memoize
    (fn []
      (-> ^js (JSS)
          (.createStyleSheet reset-jss)
          (.attach)))))

(defonce ^:private ^js page-styles
  (memoize
    (fn []
      (when (exists? js/window)
        (-> ^js (JSS)
            (.createStyleSheet #js {})
            (doto (j/call :attach)))))))

(defonce classes!
  (memoize
    (fn [styles]
      (-> (JSS)
          (j/call :createStyleSheet (clj->js styles))
          (j/call :attach)
          (j/get :classes)
          (js->clj :keywordize-keys true)))))

(def counter (volatile! 0))

(defonce class!
  (memoize
    (fn class!
      ([selector styles]
       (.addRule (page-styles) selector (clj->js styles))
       nil)
      ([styles]
       (some-> (.addRule (page-styles)
                         (str "inline-" (vswap! counter inc))
                         (clj->js styles))
               (j/get :selectorText)
               (subs 1))))))

(defn to-string [styles]
  (-> ^js (JSS)
      (.createStyleSheet (clj->js styles))
      (str)))