(ns chia.cell.lib
  (:refer-clojure :exclude [delay])
  (:require [chia.cell :as cell :refer [cell]]
            [chia.cell.runtime :as context]
            [goog.net.XhrIo :as xhr]
            [goog.net.ErrorCode :as errors]
            [chia.cell.util :as util]
            [applied-science.js-interop :as j])
  (:require-macros [chia.cell.lib])
  (:import [goog Uri]))

(defn -on-frame
  ([f] (-on-frame f nil))
  ([f initial-value]
   (let [self (first cell/*stack*)
         stop? (volatile! false)
         interval-f (cell/bound-fn frame-f []
                      (reset! self (f @self))
                      (when-not @stop?
                        (.requestAnimationFrame js/window frame-f)))]
     (context/on-dispose self #(vreset! stop? true))
     (reset! self initial-value)
     (.requestAnimationFrame js/window interval-f))))

(defn interval
  ([n f] (interval n f nil))
  ([n f initial-value]
   (if (= n :frame)
     (-on-frame f initial-value)
     (let [self (first cell/*stack*)
           clear-key (volatile! nil)
           _ (context/on-dispose self #(some-> @clear-key (js/clearInterval)))
           interval-f (cell/bound-fn [] (reset! self (f @self)))]
       (vreset! clear-key (js/setInterval interval-f n))
       (reset! self (f initial-value))))))

(defn delay
  [n value]
  (let [self (first cell/*stack*)
        clear-key (volatile! nil)
        _ (context/on-dispose self #(some-> @clear-key (js/clearTimeout)))
        timeout-f (cell/bound-fn [] (reset! self value))]
    (vreset! clear-key (js/setTimeout timeout-f n))
    nil))

(def ^:private parse-fns
  {:json->clj (comp #(js->clj % :keywordize-keys true) js/JSON.parse)
   :json js/JSON.parse
   :text identity})

(defn- xhrio-error-message [xhrio]
  (when-not (j/get xhrio :isSuccess)
    (str (-> (j/call xhrio :getLastErrorCode)
             (errors/getDebugMessage))
         \newline
         "(check your browser console for more details)")))

(defn- query-string [query]
  (-> Uri
      .-QueryData
      (.createFromMap (clj->js query)) (.toString)))

(defn fetch
  "Fetch a resource from a url. By default, response is parsed as JSON and converted to Clojure via clj->js with :keywordize-keys true.
  Accepts options :format, which may be :json or :text, and :query, a map which will be
  appended to url as a query parameter string."
  ([url]
   (fetch url {}))
  ([url {:keys [format query]
         :or {format :json->clj}}]
   (let [self (first cell/*stack*)
         url (cond-> url
                     query (str "?" (query-string query)))
         parse (get parse-fns format)]

     (cell/loading! self)

     (xhr/send url
               (cell/bound-fn [event]
                 (let [xhrio (j/get event :target)]
                   (if-let [error-message (xhrio-error-message xhrio)]
                     (cell/error! self error-message)
                     (let [formatted-value (-> (j/call xhrio :getResponseText)
                                               (parse))]
                       (cell/complete! self)
                       (reset! self formatted-value))))))
     @self)))

(defn geo-location
  []
  (let [self (first cell/*stack*)]
    (cell/loading! self)
    (-> (j/get js/navigator :geolocation)
        (j/call :getCurrentPosition
                (cell/bound-fn [location]
                  (cell/complete! self)
                  (-> (j/get location :coords)
                      (j/select-keys [:latitude :longitude])
                      (js->clj :keywordize-keys true)
                      (->> (reset! self))))
                (cell/bound-fn [error]
                  (cell/error! self (str error)))))))
