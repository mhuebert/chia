(ns chia.jss
  (:require ["jss" :as jss]
            ["reset-jss" :as reset-jss]
            ["jss-preset-default" :default jss-preset]
            [chia.view :as v]
            [chia.util.js-interop :as j]
            [chia.view.util :as vu]
            [goog.object :as gobj]))

(def JSS
  (memoize
   (fn
     ([]
      (JSS (cond-> (jss-preset)
                   (exists? js/window)
                   (j/assoc! :insertionPoint (vu/find-or-append-element "chia-jss")))))
     ([presets]
      (jss/create presets)))))

(def global-reset!
  (memoize
   (fn []
     (-> ^js (JSS)
         (.createStyleSheet reset-jss)
         (.attach)))))

(def make-classes
  (memoize
   (fn [styles]
     (-> ^js (JSS)
         (.createStyleSheet (clj->js styles))
         (.attach)
         (j/get :classes)
         (js->clj :keywordize-keys true)))))

(defn to-string [styles]
  (-> ^js (JSS)
      (.createStyleSheet (clj->js styles))
      (str)))

(defn enable-view-jss-classes! []
  (defmethod v/component-lookup :view/classes
    [this _ _]
    (some-> (v/class-get this :view/classes)
            (make-classes))))
