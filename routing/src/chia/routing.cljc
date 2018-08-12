(ns chia.routing
  (:require
   [clojure.string :as str]
   [lambdaisland.uri :as uri]
   [chia.routing.util :as u]
   #?(:cljs
      [goog.dom :as gdom]))
  #?(:cljs (:import
            [goog History]
            [goog.history Html5History]
            [goog Uri])))

(defn segments
  "Splits path into segments, ignoring leading and trailing slashes."
  [route]
  (let [segments (-> (uri/uri route)
                     :path
                     (str/split #"/" -1))
        segments (cond-> segments
                         (= "" (first segments)) (subvec 1))]
    (cond-> segments
            (= "" (last segments)) (pop))))


(comment
 (assert (= (segments "/") []))
 (assert (= (segments "//") [""]))
 (assert (= (segments "///") ["" ""]))
 (assert (= (segments "/a/b")
            (segments "a/b/")
            (segments "a/b") ["a" "b"])))

(defn query
  "Returns query parameters as map."
  [path]
  (some-> (uri/uri path)
          :query
          (u/form-decode)))

(defn parse-path
  "Returns map of parsed location information for path."
  [path]
  (let [uri (uri/uri path)]
    {:path (:path uri)
     :segments (segments (:path uri))
     :query (query uri)
     :fragment (:fragment uri)}))

;; From http://www.lispcast.com/mastering-client-side-routing-with-secretary-and-goog-history
;; Replaces this method: https://closure-library.googlecode.com/git-history/docs/local_closure_goog_history_html5history.js.source.html#line237
;; Without this patch, google closure does not handle changes to query parameters.
#?(:cljs (set! (.. Html5History -prototype -getUrl_)
               (fn [token]
                 (this-as this
                   (if (.-useFragment_ this)
                     (str "#" token)
                     (str (.-pathPrefix_ this) token))))))


(def browser?
  #?(:cljs (exists? js/window)
     :clj  false))

(def history-support?
  (when browser? #?(:cljs (.isSupported Html5History))))

#?(:cljs
   (defn get-route
     "In a browsing environment, reads the current location."
     []
     (if history-support?
       (str js/window.location.pathname js/window.location.search js/window.location.hash)
       (if (= js/window.location.pathname "/")
         (.substring js/window.location.hash 1)
         (str js/window.location.pathname js/window.location.search)))))

#?(:cljs
   (defn- make-history
     "Set up browser history, using HTML5 history if available."
     []
     (when browser?
       (if history-support?
         (doto (Html5History.)
           (.setPathPrefix (str js/window.location.protocol
                                "//"
                                js/window.location.host))
           (.setUseFragment false))
         (if (not= "/" js/window.location.pathname)
           (set! (.-location js/window) (str "/#" (get-route)))
           (History.))))))

#?(:cljs
   (defonce history
            (some-> (make-history)
                    (doto (.setEnabled true)))))

#?(:cljs
   (defn nav!
     "Trigger pushstate navigation to token (path)"
     ([route] (nav! route true))
     ([route add-history-state?]
      (if add-history-state?
        (.setToken history route)
        (.replaceToken history route)))))

(defn query-string
  "Returns query string, including '?'. Removes empty values. Returns nil if empty."
  [m]
  (some->> (u/remove-empty m)
           (u/form-encode)
           (u/some-str)))

#?(:cljs
   (defn query-nav!
     "Navigates to current route with query-string replaced by the provided `query` map."
     [query]
     (let [location (.-location js/window)]
       (-> (uri/map->URI {:path (.-pathname location)
                          :fragment (.-hash location)
                          :query (query-string query)})
           (str)
           (nav!)))))

#?(:cljs
   (defn swap-query!
     "Navigates to current route with query parameters modified by `f`,
      which is passed the current query-map followed by `args`."
     [f & args]
     (-> (apply f (query (get-route)) args)
         (query-nav!))))

#?(:cljs
   (defn link?
     "Return true if element is a link"
     [el]
     (= "A" (some-> el (.-tagName)))))

#?(:cljs
   (defn closest
     "Return element or first ancestor of element that matches predicate, like jQuery's .closest()."
     [el pred]
     (if (pred el)
       el
       (gdom/getAncestor el pred))))

#?(:cljs
   (defn click-event-handler
     "Intercept clicks on links with valid pushstate hrefs. Callback is passed the link's href value."
     [callback e]
     (when-let [link (closest (.-target e) link?)]
       (let [location ^js (.-location js/window)
             ;; in IE/Edge, link elements do not expose an `origin` attribute
             origin (str (.-protocol location)
                         "//"
                         (.-host location))
             ;; check to see if we should let the browser handle the link
             ;; (eg. external link, or valid hash reference to an element on the page)
             handle-natively? (or (not= (.-host location) (.-host link))
                                  (not= (.-protocol location) (.-protocol link))
                                  ;; if only the hash has changed, & element exists on page, allow browser to scroll there
                                  (and (.-hash link)
                                       (= (.-pathname location) (.-pathname link))
                                       (not= (.-hash location) (.-hash link))
                                       (.getElementById js/document (subs (.-hash link) 1))))]
         (when-not handle-natively?
           (.preventDefault e)
           (callback (str/replace (.-href link) origin "")))))))

#?(:cljs
   (def intercept-clicks
     "Intercept local links (handle with router instead of reloading page)."
     (memoize                                               ;; only do this once per page
      (fn intercept
        ([]
         (when browser?
           (intercept js/document)))
        ([element]
         (when browser?
           (.addEventListener element "click" (partial click-event-handler nav!))))))))

#?(:cljs
   (defn listen
     "Set up a listener on route changes. Options:

     intercept-clicks? (boolean, `true`): For `click` events on local links, prevent page reload & fire listener instead.
     fire-now? (boolean, `true`): executes listener immediately, with current parsed route.

     Returns a key which can be passed to `unlisten` to remove listener."
     ([listener]
      (listen listener {}))
     ([listener {:keys [fire-now?
                        intercept-clicks?]
                 :or {fire-now? true
                      intercept-clicks? true}}]
      (when intercept-clicks? (intercept-clicks))
      (when fire-now? (listener (parse-path (get-route))))
      (let [cb #(listener (parse-path (get-route)))]
        (.addEventListener history "navigate" cb)
        ;; return the callback, for unlisten purposes
        cb))))

#?(:cljs
   (defn unlisten [cb]
     (.removeEventListener history "navigate" cb)))

