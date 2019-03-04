(ns chia.view.render-loop
  (:require ["react" :as react]
            ["react-dom" :as react-dom]
            [applied-science.js-interop :as j]
            [chia.view.fps :as fps]
            [chia.util :as u]))


(defonce ^:dynamic *immediate-state-update* false)

;; raf polyfill
(when (and (exists? js/window)
           (not (j/get js/window :requestAnimationFrame)))
  (j/assoc! js/window :requestAnimationFrame
            (or
              (j/get js/window :webkitRequestAnimationFrame)
              (j/get js/window :mozRequestAnimationFrame)
              (j/get js/window :oRequestAnimationFrame)
              (j/get js/window :msRequestAnimationFrame)
              (fn [cb]
                (js/setTimeout cb (/ 1000 60))))))

(defonce to-render (volatile! #{}))
(declare request-render)

(defn forget! [component]
  (j/assoc! component .-chia$toUpdate false))

(defprotocol IForceUpdate
  (-force-update! [this]))

(defn force-update!
  "Force-updates `component` immediately."
  [^js component]
  (vswap! to-render disj component)
  (-force-update! component))

(defn schedule-update!
  "Queues a force-update for `component`"
  [^js component]
  (if (true? *immediate-state-update*)
    (force-update! component)
    (do
      (j/assoc! component .-chia$toUpdate true)
      (vswap! to-render conj component)
      (request-render))))

(defn order [component]
  (j/get component .-chia$order))

(defn flush!
  []
  (when-let [components (seq @to-render)]
    (vreset! to-render #{})
    (doseq [c (sort-by order components)]
      (when (and ^boolean (j/get c .-chia$toUpdate)
                 (not ^boolean (j/get c .-chia$unmounted)))
        (-force-update! c)))))

(defn render-loop
  [frame-ms]
  ;(fps/tick! frame-ms)
  (flush!))

(defn request-render []
  (js/requestAnimationFrame render-loop))


(defn apply-sync!
  "Wraps function `f` to flush the render loop before returning."
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (flush!)
      result)))