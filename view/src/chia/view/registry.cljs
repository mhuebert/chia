(ns chia.view.registry)

(defonce registry-ref (atom {}))

(defn register-view! [view-var]
  (when js/goog.DEBUG
    (let [{:as var-meta
           :keys [view/name
                  doc]} (meta view-var)]
      (swap! registry-ref assoc name
             (merge
              (select-keys var-meta [:view/name
                                     :view/arglist])
              (when doc {:doc doc})
              {:view @view-var})))))
