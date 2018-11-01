(ns chia.graphql.request
  (:refer-clojure :exclude [catch])
  (:require ["unfetch" :as unfetch]
            [chia.graphql.cache :as cache]
            [chia.util.js-interop :as j]))

(defn promise? [x]
  (= (js/Promise.resolve x) x))

(defn ->promise ^js [x]
  (cond-> x
          (not (promise? x)) (js/Promise.resolved)))

(defn then [x f]
  (.then (->promise x) f))

(defn catch [^js x f]
  (.catch x f))

(defn get-token [token]
  (cond (promise? token) (then token get-token)
        (fn? token) (get-token (token))
        :else (js/Promise.resolve token)))

(defn handle-error [req error]
  (cache/merge-query-meta! req {:async/error {:message "Error sending GraphQL operation"
                                              :error error}}))

(defn fetch [url options]
  (unfetch url (-> options
                   (update :body (comp js/JSON.stringify clj->js))
                   (clj->js))))

(defn post!
  [{:as api-params
    :keys [token
           url]}
   {:as req
    :keys [query
           variables
           callback]}]
  (when callback
    (throw (ex-info "Did not expect callback" {:req req})))

  (cache/merge-query-meta! req {:async/loading? true})

  (-> (get-token token)
      (then
       (fn [token]
         (fetch url
                {:method "POST"
                 :credentials "include"
                 :headers (cond-> {:Content-Type "application/json"}
                                  token (assoc :Authorization (str "Bearer: " token)))
                 :body {:query (str query)
                        :variables variables}})))
      (then #(j/call % :json))
      (then (fn [response]
                (let [{:keys [async/error]
                       :as result} (cache/write-response! req response)]
                  result)))
      (catch (partial handle-error req))))