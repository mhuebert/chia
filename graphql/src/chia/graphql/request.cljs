(ns chia.graphql.request
  (:require ["unfetch" :as unfetch]
            [chia.graphql.cache :as cache]
            [promesa.core :as p]))

(defn promise? [x]
  (= (js/Promise.resolve x) x))

(defn get-token [token]
  (cond (promise? token) (.then token get-token)
        (fn? token) (get-token (token))
        :else (p/resolved token)))

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

  (cache/merge-query-meta! req {:async/loading? true})

  (-> (get-token token)
      (p/then
       (fn [token]
         (fetch url
                {:method "POST"
                 :credentials "include"
                 :headers (cond-> {:Content-Type "application/json"}
                                  token (assoc :Authorization (str "Bearer: " token)))
                 :body {:query (str query)
                        :variables variables}})))
      (p/then (fn [^js res]
                (.json res)))
      (p/then (fn [^js response]
                (let [{:keys [async/value
                              async/error]
                       :as result} (cache/write-response! req response)]
                  (when callback
                    (callback error value))
                  result)))
      (p/catch (fn [err]
                 (handle-error req err)
                 (when callback
                   (callback err))))))