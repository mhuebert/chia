(ns chia.view.render-loop
  (:require ["react" :as react]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as j]
            [chia.view.fps :as fps]
            [chia.view.impl :as impl]
            [chia.util :as u]))

(impl/raf-polyfill!)

(defonce ^:dynamic *immediate-updates*
         ;; When true, updates will not be queued.
         false)

(defonce to-render (volatile! #{}))

(defn- order [view]
  (j/get view .-chia$order))

(defn dequeue! [view]
  (j/assoc! view .-chia$toUpdate false))

;;;;;;;;;;;;;;;;;;
;;
;; Render loop

(defn flush!
  "Render all queued updates immediately."
  []
  (when-let [views (seq @to-render)]
    (vreset! to-render #{})
    (doseq [c (sort-by order views)]
      (when (and ^boolean (j/get c .-chia$toUpdate)
                 (not ^boolean (j/get c .-chia$unmounted)))
        (j/call c :forceUpdate)))))

(defn force-update!
  "Force-updates `view` immediately."
  [view]
  (vswap! to-render disj view)
  (j/call view :forceUpdate))

(defn schedule-update!
  [view]
  "Queues a force-update for `component`"
  (if (true? *immediate-updates*)
    (force-update! view)
    (do
      (j/assoc! view .-chia$toUpdate true)
      (vswap! to-render conj view)
      (js/requestAnimationFrame flush!))))

(defn apply-sync!
  "Wraps function `f` to flush the render loop before returning."
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (flush!)
      result)))