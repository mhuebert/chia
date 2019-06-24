(ns chia.jss
  (:require ["jss" :as jss]
            ["reset-jss" :as reset-jss]
            ["jss-preset-default" :default jss-preset]
            [applied-science.js-interop :as j]
            [chia.view.util :as vu]
            [chia.util :as u]))

(defonce JSS
  (delay
    (jss/create (jss-preset))))

(defonce global-reset!
  (delay
    (when (exists? js/window)
      (-> @JSS
          (j/call :createStyleSheet reset-jss)
          (doto (j/call :attach))))))

(defonce ^:private page-styles
  (delay
    (when (exists? js/window)
      (-> @JSS
          (j/call :createStyleSheet #js {} #js{:meta (str ::page-styles)})
          (doto (j/call :attach))))))

(defonce classes!
  (memoize
    (fn [styles]
      (-> @page-styles
          (doto (j/call :addRules (clj->js styles)))
          (j/get :classes)
          (js->clj :keywordize-keys true)))))

(defonce counter (volatile! 0))

(defonce class!
  (memoize
    (fn [styles]
      (some-> @page-styles
              (j/call :addRule (str "inline-" (vswap! counter inc)) (clj->js styles))
              (j/get :selectorText)
              (subs 1)))))

(defn to-string [styles]
  (-> @JSS
      (j/call :createStyleSheet (clj->js styles))
      (str)))