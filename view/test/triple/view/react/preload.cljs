(ns triple.view.react.preload
  (:require ["react-refresh/runtime" :as refresh]))

(goog/exportSymbol "ReactRefresh" refresh)

(comment
  ;; in your app:
  (when (exists? js/ReactRefresh)
    (.injectIntoGlobalHook js/ReactRefresh goog/global)))

(defn ^:dev/after-load refresh!
  []
  (->> (refresh/performReactRefresh)
       (js/console.log "refreshed:")))