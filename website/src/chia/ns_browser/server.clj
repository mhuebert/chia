(ns chia.ns-browser.server
  (:require [static.assets :as assets]
            [shadow.http.push-state :as push-state]
            [shadow.cljs.devtools.server.runtime :as runtime]
            [shadow.cljs.devtools.server.supervisor :as super]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]
            [chia.routing :as routing]
            [clojure.string :as str]


            [ring.util.mime-type :refer [ext-mime-type]]))

(defn inspect [x]
  (cond (keyword? x) x

        (map? x)
        (reduce-kv (fn [m k v]
                     (assoc m k (inspect v))) {} x)

        :else (type x)))

(defn build-env [build-id]
  (some-> (runtime/get-instance!)
          :supervisor
          (super/get-worker build-id)
          :state-ref
          (deref)
          :build-state
          :compiler-env))

(defn serve-resources [{:as req
                        :keys [build-id uri]}]
  (let [segments (routing/segments uri)
        result (push-state/handle req)]
    (match segments
           ["_" "resource" & whatever] (when-let [resource (some->> (io/resource (str/join \/ whatever))
                                                                    (assets/try-slurp))]
                                         {:body resource
                                          :status 200
                                          :headers {"content-type" (or (ext-mime-type uri)
                                                                       "text/plain")}})
           :else result)))


(comment

 (def proc (watch))

 (def output-chan
   (let [c (chan)]
     (tap (:output-mult proc) c)
     c))

 (defn bootstrap-compiled-sources [{:keys [type build-config info]}]
   (when (and (= :build-complete type)
              (= :bootstrap (:target build-config)))
     (:compiled info)))

 (def bootstrap-loop
   (go-loop []
     (let [evt (<! output-chan)]
       (some-> (bootstrap-compiled-sources evt)
               (println))
       (recur))))

 (put! output-chan false)
 (comment
  :type #{:build-log}
  :event {:type {:shadow.build/process-stage {:target #{:bootstrap}
                                              :stage #{:resolve
                                                       :compile-prepare}}
                 [:cache-write
                  :cache-read] [:resource-id, :resource-name]
                 :compile-cljs {:resource-id #{[:shadow.build.targets.bootstrap/macro chia.view]}
                                :resource-name "chia.view.class$macros.cljc"}
                 :build-complete {:build-config {}
                                  :info {:compile-start :Number
                                         :timings {[:resolve
                                                    :compile-prepare
                                                    :compile-finish
                                                    :flush] {}}
                                         :modules []
                                         :sources []
                                         :compiled #{[:shadow.build.targets.bootstrap/macro chia.view]
                                                     [:shadow.build.classpath/resource "chia_demo/website.cljs"]
                                                     [:shadow.build.classpath/resource "chia/reactive.cljc"]}}}}})

 #_(close! output-chan)
 (close! output-loop)
 )
