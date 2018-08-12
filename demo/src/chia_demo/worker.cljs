(ns chia-demo.worker
  (:require ["serviceworker-cache-polyfill"]
            [clojure.string :as str]))
;(js/importScripts "https://storage.googleapis.com/workbox-cdn/releases/3.2.0/workbox-sw.js")
;
;(def workbox js/workbox)
;
;(def core (.-core workbox))
;
;(.setCacheNameDetails core #js {:prefix "chia-demo"
;                                :suffix "v1"})

(def env {:cache-name "pwa-chia"
          :files-to-cache ["/compiled/app.js"
                           "/compiled/app.css"
                           "/compiled/worker.js"
                           "/index.html"
                           "/"]})

(defn on-install [event]
  (prn :install!)
  (.waitUntil event
              (-> js/caches
                  (.open (env :cache-name))
                  (.then (fn [cache]
                           (-> cache
                               (.addAll (clj->js (env :files-to-cache)))))))))

(defn network-and-cache [cache req return?]
  (-> req
      (js/fetch)
      (.then
       (fn [res]
         (.put cache req (cond-> res
                                 return? (.clone)))
         res))))

(defn cache-then-network [cache req]
  (-> cache
      (.match req)
      (.then (fn [res]
               (if res
                 (do (network-and-cache cache req false)
                     res)
                 (network-and-cache cache req true))))))

(defn strip-compiled-query-params [url]
  (cond-> url
          (re-matches #".*/compiled/.*" url)
          (str/replace #"\?.*" "")))

(defn on-fetch [^js event]
  #_(aset event "request" "url"
          (strip-compiled-query-params (.. event -request -url)))
  (let [req (.-request event)]
    (-> event
        (.respondWith
         (-> js/caches
             (.open (env :cache-name))
             (.then (fn [cache]
                      (cache-then-network cache req))))))))

(doto js/self
  (.addEventListener "install" on-install)
  (.addEventListener "fetch" on-fetch))

(prn :HELLO)
