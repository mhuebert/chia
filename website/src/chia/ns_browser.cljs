(ns chia.ns-browser
  (:refer-clojure :exclude [require])
  (:require [shadow.cljs.bootstrap.browser :as boot]
            [shadow.cljs.devtools.client.browser :as shadow-client]
            [cljs.js :as cljs]
            [clojure.spec.test.alpha :as st]
            [chia.view :as v]
            [chia.view.util :refer [find-or-append-element]]
            [chia.ns-browser.views :as carton :refer [carton]]
            [goog.functions :as gf]
            [clojure.set :as set]
            [cljs.tagged-literals :as tags]
            [shadow.cljs.bootstrap.env :as bootstrap-env]
            [shadow.cljs.devtools.client.env :as env]
            [goog.net.XhrIo :as xhr]
            [clojure.string :as str]
            [cljs.reader :as reader]))

(def bootstrap-path "/compiled/bootstrap")

(defn load [& args]
  (prn args)
  (apply boot/load carton/compiler args))

(def eval-opts {:load (partial boot/load carton/compiler)
                :eval cljs/js-eval})

(defn eval-form

  ([form]
   (binding [cljs/*eval-fn* cljs/js-eval]
     (eval-form form (fn [{:keys [value error]}]
                       (if error (throw error)
                                 (do
                                   (prn :evaled form)
                                   (js/console.log "out" value)))))))
  ([form cb]
   (binding [cljs/*eval-fn* cljs/js-eval]
     (cljs/eval carton/compiler form eval-opts cb))))

(defn ^:dev/after-load render []
  (v/render-to-dom (carton {:view/state carton/compiler})
                   (find-or-append-element "chia-egg")))

(defn load-cljs-resource! [{path :resource-name
                            ns-name :ns}]
  (xhr/send (str "/chia/resource/" path)
            (fn [e]
              (this-as req
                (swap! carton/compiler update :cljs.analyzer/namespaces dissoc ns-name)
                (cljs/eval-str carton/compiler
                               (.getResponseText req)
                               ns-name
                               eval-opts
                               println)))))

(defn with-index [cb]
  ;; TODO
  ;; load index form environment
  (boot/transit-load
   (boot/asset-path "/index.transit.json")
   (fn [data]
     (bootstrap-env/build-index data)
     (cb))))

#_(defn merge-caches! [sources]
    (swap! carton/compiler update :cljs.analyzer/namespaces merge
           (->> sources
                (reduce (fn [m {:keys [ns analysis-cache]}]
                          (cond-> m
                                  analysis-cache (assoc ns analysis-cache))) {})))
    sources)

(defn handle-build-complete [{{:keys [compiled]} :info}]
  (with-index
   (fn []
     (let [active-namespaces (set (keys (:cljs.analyzer/namespaces @carton/compiler)))
           compiled-namespaces (-> (:sources @bootstrap-env/index-ref)
                                   (select-keys compiled)
                                   (vals)
                                   (->> (filter (comp active-namespaces :ns))))]
       (shadow-client/load-sources
        compiled-namespaces
        (fn [sources]
          (doseq [{ns-name :ns
                   :keys [resource-name]
                   :as resource} sources]
            (load-cljs-resource! resource))
          #_(shadow-client/do-js-load sources)
          #_(merge-caches! sources)
          (render)))))))

(defn monkey-patch-shadow-build-complete []
  (let [shadow-handle-build-complete shadow-client/handle-build-complete]
    (set! shadow-client/handle-build-complete
          (fn [msg]
            (shadow-handle-build-complete msg)
            (#'handle-build-complete msg)))))

(defn init []
  (monkey-patch-shadow-build-complete)
  (add-watch carton/compiler :knowledge
             (gf/debounce (fn [key ref before after]
                            (prn :compiler-swapped :changed? (not= before after))) 200))
  (boot/init carton/compiler
             {:path bootstrap-path
              :load-on-init '#{chia-demo.app chia.ns-browser}}
             (fn [& args]

               (st/instrument)

               (swap! carton/compiler
                      update
                      :cljs.analyzer/namespaces
                      (partial into (sorted-map)))

               (eval-form '(chia-demo.app/render))

               (render)))

  #_(some-> @shadow-client/socket-ref
            (.addEventListener "message" handle-message)))

