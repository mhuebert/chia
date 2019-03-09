(ns chia.routing
  (:require
    [clojure.string :as str]
    [lambdaisland.uri :as uri]
    [chia.routing.util :as u]
    #?(:cljs
       [goog.dom :as gdom])
    [applied-science.js-interop :as j])
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
    {:path     (:path uri)
     :segments (segments (:path uri))
     :query    (query uri)
     :fragment (:fragment uri)}))

(def browser?
  #?(:cljs (exists? js/window)
     :clj  false))

(def history-support?
  (when browser? #?(:cljs (.isSupported Html5History))))

(defn query-string
  "Returns query string, including '?'. Removes empty values. Returns nil if empty."
  [m]
  (some->> (u/remove-empty m)
           (u/form-encode)
           (u/some-str)))

#?(:cljs
   (do

     ;; From http://www.lispcast.com/mastering-client-side-routing-with-secretary-and-goog-history
     ;; Replaces this method: https://closure-library.googlecode.com/git-history/docs/local_closure_goog_history_html5history.js.source.html#line237
     ;; Without this patch, google closure does not handle changes to query parameters.
     (set! (.. Html5History -prototype -getUrl_)
           (fn [token]
             (this-as this
               (if (.-useFragment_ this)
                 (str "#" token)
                 (str (.-pathPrefix_ this) token)))))

     (defn get-route
       "In a browsing environment, reads the current location."
       []
       (if history-support?
         (str js/window.location.pathname js/window.location.search js/window.location.hash)
         (if (= js/window.location.pathname "/")
           (.substring js/window.location.hash 1)
           (str js/window.location.pathname js/window.location.search))))

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
             (History.)))))

     (defonce history
       (some-> (make-history)
               (doto (.setEnabled true))))

     (defn nav!
       "Trigger pushstate navigation to token (path)"
       ([route] (nav! route true))
       ([route add-history-state?]
        (if add-history-state?
          (.setToken history route)
          (.replaceToken history route))))

     (defn query-nav!
       "Navigates to current route with query-string replaced by the provided `query` map."
       [query]
       (let [location (.-location js/window)]
         (-> (uri/map->URI {:path     (.-pathname location)
                            :fragment (.-hash location)
                            :query    (query-string query)})
             (str)
             (nav!))))

     (defn swap-query!
       "Navigates to current route with query parameters modified by `f`,
        which is passed the current query-map followed by `args`."
       [f & args]
       (-> (apply f (query (get-route)) args)
           (query-nav!)))

     (defn link?
       "Return true if element is a link"
       [el]
       (= "A" (some-> el (.-tagName))))

     (defn closest
       "Return element or first ancestor of element that matches predicate, like jQuery's .closest()."
       [el pred]
       (if (pred el)
         el
         (gdom/getAncestor el pred)))

     (def ^:dynamic *click-event* nil)

     (defn external? [link-element]
       (let [{current-host     :host
              current-protocol :protocol} (j/lookup (.-location js/window))
             {link-host     :host
              link-protocol :protocol
              link-path     :pathname} (j/lookup link-element)]
         (or (not= current-host link-host)
             (not= current-protocol link-protocol)
             (re-find #"\.[^/]+$" link-path))))

     (defn valid-anchor? [link-element]
       (let [^js location (.-location js/window)]
         (and (.-hash link-element)
              (= (.-pathname location) (.-pathname link-element))
              (not= (.-hash location) (.-hash link-element))
              (.getElementById js/document (subs (.-hash link-element) 1)))))

     (defn click-event-handler
       "Intercept clicks on links with valid pushstate hrefs. Callback is passed the link's href value."
       [callback ^js e]
       (when-let [link-element (closest (.-target e) link?)]
         (let [ignore-click? (or (external? link-element)
                                 (valid-anchor? link-element)
                                 (.-defaultPrevented e))]
           (when-not ignore-click?
             (.preventDefault e)
             (binding [*click-event* e]
               (let [location ^js (.-location js/window)
                     ;; in IE/Edge, link elements do not expose an `origin` attribute
                     origin (str (.-protocol location)
                                 "//"
                                 (.-host location))]
                 (callback (str/replace (.-href link-element) origin ""))))))))

     (def root-click-listener (partial click-event-handler nav!))

     (defonce intercept-clicks
       ; Intercept local links (handle with router instead of reloading page).
       (memoize
         (fn intercept
           ([]
            (when browser?
              (intercept js/document)))
           ([element]
            (when browser?
              (.addEventListener element "click" root-click-listener))))))

     (defonce ^:private listeners (atom #{}))

     (defn- add-listener! [f]
       (swap! listeners conj f))

     (defn unlisten [f]
       (swap! listeners disj f))

     (defn ^:dev/after-load fire!
       ([] (fire! @listeners))
       ([listener-fns]
        (let [route-state (-> (get-route)
                              (parse-path)
                              (cond-> *click-event*
                                      (assoc :click-event *click-event*)))]
          (doseq [listener listener-fns]
            (listener route-state)))))

     (defonce start!
       (memoize
         (fn []
           (.addEventListener history "navigate" #(fire!)))))

     (defn listen
       "Set up a listener on route changes. Options:

       intercept-clicks? (boolean, `true`): For `click` events on local links, prevent page reload & fire listener instead.
       fire-now? (boolean, `true`): executes listener immediately, with current parsed route.

       Returns a key which can be passed to `unlisten` to remove listener."
       ([listener]
        (listen listener {}))
       ([listener {:keys [fire-now?
                          intercept-clicks?]
                   :or   {fire-now?         true
                          intercept-clicks? true}}]

        (start!)

        (when intercept-clicks?
          (intercept-clicks))

        (swap! listeners conj listener)

        (when fire-now?
          (fire! #{listener}))

        listener))))

