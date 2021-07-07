(ns triple.view.react.hooks
  (:require ["react" :as react]
            [applied-science.js-interop :as j]))

(def dispatcher (j/!get-in react [:__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED :ReactCurrentDispatcher]))

(defn current-view? []
  (some? (j/!get dispatcher :current)))

(defn memo-once [init-fn]
  (let [ref (react/useRef)
        value (j/!get ref :current)]
    (if (undefined? value)
      (let [v (init-fn)]
        (j/!set ref :current v)
        v)
      value)))
