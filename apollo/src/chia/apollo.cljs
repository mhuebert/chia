(ns chia.apollo
  (:require ["apollo-client" :refer [ApolloClient]]
            ["apollo-link" :refer [ApolloLink]]
            ["apollo-link-context" :refer [setContext]]
            ["apollo-link-http" :refer [HttpLink]]
            ["apollo-link-error" :refer [onError]]
            ["apollo-link-state" :refer [withClientState]]
            ["apollo-cache-inmemory" :refer [InMemoryCache IntrospectionFragmentMatcher]]
            ["apollo-cache-persist" :refer [persistCache]]

            [applied-science.js-interop :as j]
            [clojure.string :as str]))

(defn data->clj
  "Convert apollo data object to clj. (plain js->clj does not work on these objects.)"
  [data]
  (some->> data
           (js/Object.keys)
           (reduce (fn [m k]
                     (cond-> m
                             (string? k)
                             (assoc (keyword k) (js->clj (j/get data ^String k) :keywordize-keys true))))
                   {})))

(defn default-error-handler [^js info]
  (if-let [gql-errors (.-graphQLErrors info)]
    (doseq [k (js/Object.keys gql-errors)]
      (let [error (j/get gql-errors k)]
        (-> (str "[GraphQL error]: Message: " (j/get error :message) ", "
                 "Location: " (js->clj (j/get error :locations)) ", "
                 "Path: " (j/get error :path))
            (js/console.log))))
    (when-let [network-error (.-networkError info)]
      (js/console.error network-error))))

(def Client (atom nil))
(def Cache (atom nil))

(defn init-client! [{:keys [uri
                            get-auth-token
                            on-error

                            cache
                            cache/storage
                            cache/persist?
                            cache/resolvers]
                     :or {on-error default-error-handler
                          persist? false
                          storage (.-localStorage js/window)}}]
  (reset! Cache cache)
  (js/Promise.
   (fn [resolve reject]
     (let [http-link (when uri
                       (HttpLink.
                        #js {:uri uri}))
           state-link (when cache
                        (withClientState (-> {:cache cache}
                                             (merge resolvers)
                                             (clj->js))))
           auth-link (when get-auth-token
                       (setContext
                        (fn [_ ^js res]
                          (-> (get-auth-token)
                              (.then
                               (fn [token]
                                 #js {:headers (doto (or (.-headers res) #js {})
                                                 (j/assoc! :authorization
                                                           (if token (str "Bearer: " token) "")))}))))))
           error-link (onError on-error)]
       (reset! Client (ApolloClient. #js {:link (->> [error-link
                                                      state-link
                                                      auth-link
                                                      http-link]
                                                     (keep identity)
                                                     (to-array)
                                                     (.from ApolloLink))
                                          :cache cache})))
     (if persist?
       (-> (persistCache #js {:cache cache
                              :storage (.-localStorage js/window)})
           (.then resolve)
           (.catch
            (fn [error]
              (reject error))))
       (resolve)))))

(defn read-query [query variables]
  (-> query
      (.readQuery (cond-> #js {:query @query}
                          variables (j/assoc! :variables (clj->js variables))))
      (data->clj)))

(defn write-fragment! [fragment id data]
  (.writeFragment @Client #js {:id id
                               :fragment @fragment
                               :fragmentName (name fragment)
                               :data (clj->js data)}))

(defn update-fragment! [fragment id f & args]
  (let [oldval (.readFragment @Client #js {:id id
                                           :fragment @fragment
                                           :fragmentName (name fragment)})]
    (.writeFragment @Client
                    #js {:id id
                         :fragment @fragment
                         :fragmentName (name fragment)
                         :data (apply f oldval args)})))


