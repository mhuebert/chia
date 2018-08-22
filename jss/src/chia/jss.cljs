(ns chia.jss
  (:require ["jss" :as jss]
            ["reset-jss" :as reset-jss]
            ["jss-preset-default" :default jss-preset]
            [chia.util.js-interop :as j]
            [chia.view.util :as vu]
            [goog.object :as gobj]))

(def ^js JSS
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
     (-> (JSS)
         (.createStyleSheet reset-jss)
         (.attach)))))

(def make-classes
  (memoize
   (fn [styles]
     (-> (.createStyleSheet (JSS) (clj->js styles))
         (.attach)
         (j/get :classes)
         (js->clj :keywordize-keys true)))))

(defn to-string [styles]
  (-> (JSS)
      (.createStyleSheet (clj->js styles))
      (str)))

;; if chia.view is present, enable :view/classes
(when-let [component-lookup (try @(resolve 'chia.view/component-lookup)
                                 (catch js/Error e nil))]
  (let [class-get @(resolve 'chia.view/class-get)]
    (defmethod component-lookup :view/classes
      [this _ _]
      (some-> (class-get this :view/classes)
              (make-classes)))))
