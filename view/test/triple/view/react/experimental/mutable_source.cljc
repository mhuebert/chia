(ns triple.view.react.experimental.mutable-source
  (:refer-clojure :exclude [use])
  (:require ["react" :as react]
            [applied-science.js-interop :as j]))

(defn feature-enabled? []
  (some-> (j/!get-in react [:__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED
                            :ReactCurrentDispatcher
                            :current])
          (j/contains? :createMutableSource)))

(defn create [store get-version]
  (if (feature-enabled?)
    (doto (react/createMutableSource store get-version)
      (-> (js-keys) (prn :CMS)))
    (j/obj :store store)))

(defn use [source get-snapshot subscribe]
  (if (feature-enabled?)
    (react/useMutableSource source get-snapshot subscribe)
    (j/let [^:js {:keys [store]} source
            ^:js [value set-value] (react/useState #(get-snapshot store))]
      (react/useLayoutEffect #(subscribe store (fn [] (set-value (get-snapshot store))))
                             #js[get-snapshot])
      value)))