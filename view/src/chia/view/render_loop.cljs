(ns chia.view.render-loop
  (:require ["react" :as react]
            ["react-dom" :as react-dom]))

(set! *warn-on-infer* true)
(defonce ^:dynamic *immediate-state-update* false)


(goog-define count-fps?* false)                             ;; allows setting a default at compile time

(defonce ^:private count-fps? count-fps?*)
(defonce ^:private last-fps-time 1)
(defonce frame-rate 0)
(defonce frame-count 0)

(defonce fps-element
         (memoize (fn []
                    (-> js/document.body
                        (.appendChild (doto (js/document.createElement "div")
                                        (.setAttribute "style"  "padding: 3px 3px 0 0; font-size: 9px;")
                                        (.setAttribute "class" "fixed top-0 right-0 z-max monospace gray")))))))

(defn render-fps []
  (react-dom/render (react/createElement "div" #js {} (str (Math.floor frame-rate)))
                    (fps-element)))

(defn measure-frame-rate!
  [value]
  (set! count-fps? value))

(defonce _raf-polyfill
         (when (js* "typeof window !== 'undefined'")
           (if-not (.-requestAnimationFrame js/window)
             (set! (.-requestAnimationFrame js/window)
                   (or
                    (.-webkitRequestAnimationFrame js/window)
                    (.-mozRequestAnimationFrame js/window)
                    (.-oRequestAnimationFrame js/window)
                    (.-msRequestAnimationFrame js/window)
                    (fn [cb]
                      (js/setTimeout cb (/ 1000 60))))))))

(defonce to-render (volatile! #{}))
(defonce to-run (volatile! []))

(declare request-render)

(defn schedule! [f]
  (vswap! to-run conj f)
  (request-render))

(defn force-update!* [^js this]
  (when-not (true? (.-unmounted this))
    (.forceUpdate this)))

(defn force-update!
  "Force-updates `component` immediately."
  [^js component]
  (vswap! to-render disj component)
  (force-update!* component))

(defn force-update
  "Queues a force-update for `component`"
  [^js component]
  (if (true? *immediate-state-update*)
    (force-update! component)
    (do
      (set! (.-chia$toUpdate component) true)
      (vswap! to-render conj component)
      (request-render))))

(defn order [^js component]
  (.-chia$order component))

(defn flush!
  []
  (when-not ^boolean (empty? @to-render)
    (let [components @to-render]
      (vreset! to-render #{})
      (doseq [^js c (sort-by order components)]
        (when ^boolean (.-chia$toUpdate c)
          (force-update!* c)))))

  (when-not ^boolean (empty? @to-run)
    (let [fns @to-run]
      (vreset! to-run [])
      (doseq [f fns] (f)))))

(defn render-loop
  [frame-ms]
  (when ^boolean (true? count-fps?)
    (set! frame-count (inc frame-count))
    (when (identical? 0 (mod frame-count 29))
      (set! frame-rate (* 1000 (/ 30 (- frame-ms last-fps-time))))
      (set! last-fps-time frame-ms)
      (render-fps)))
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