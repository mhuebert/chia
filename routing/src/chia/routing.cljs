(ns chia.routing
  (:require [clojure.string :as str]
            [chia.util.string :as string]
            [applied-science.js-interop :as j]))

(defonce ^:private listeners (atom {}))
(declare ^:private fire!)

(def URL (when (exists? js/URL) js/URL))
(def URLSearchParams (when (exists? js/URLSearchParams)
                       js/URLSearchParams))

(defn url ^js [route]
  (if (instance? URL route)
    route
    (new URL (str (when-not (str/starts-with? route "http")
                    "http://x.y/")
                  (string/strip-prefix route "/")))))

(def encode (comp js/encodeURIComponent name))
(def decode (comp js/decodeURIComponent name))

(defn segments
  "Splits path into segments, ignoring leading and trailing slashes."
  [path]
  (let [segments (-> (url path)
                     (j/get :pathname)
                     (str/split #"/" -1))]
    (cond-> segments
            (= "" (first segments)) (subvec 1)
            (= "" (last segments)) (pop))))

(defn query
  "Returns query parameters as map."
  [path]
  (let [URL (url path)]
    (when-let [^js params (j/get URL :searchParams)]
      (reduce (fn [m k]
                (assoc m (keyword k) (.get params k)))
              {}
              (js/Array.from (.keys params))))))

(defn remove-empty
  "Remove empty values/strings from map"
  [m]
  (reduce-kv (fn [m k v]
               (cond-> m
                       (or (nil? v)
                           (= "" v)
                           (and (satisfies? ISeqable v)
                                (empty? v))) (dissoc k))) m m))

(defn query-string
  "Returns query string, including '?'. Removes empty values. Returns nil if empty."
  [m]
  (some->> (remove-empty m)
           (reduce-kv (fn [out k v]
                        (str out (encode k) (encode v))) "")
           (str "?")))

(defn parse-path
  "Returns map of parsed location information for path."
  [path]
  (let [URL (url path)]
    {:path     (j/get URL :pathname)
     :fragment (j/get URL :hash)
     :segments (segments URL)
     :query    (query URL)}))

(def browser? (exists? js/window))

(def history-support?
  (and (exists? js/history) (j/contains? js/history :pushState)))

(defn get-route
  "In a browsing environment, reads the current location."
  []
  (let [{:keys [pathname search hash]} (j/lookup js/location)]
    (if history-support?
      (str pathname search hash)
      (if (= pathname "/")
        (.substring hash 1)
        (str pathname search)))))

(defn nav!
  "Trigger pushstate navigation to token (path)"
  ([route] (nav! route true))
  ([route add-history-state?]
   (if add-history-state?
     (do (.pushState js/history nil "" route)
         (fire!))
     (.replaceState js/history nil "" route))))

(defn query-nav!
  "Navigates to current route with query-string replaced by the provided `query` map."
  [query]
  (let [params (reduce-kv (fn [params k v]
                            (doto ^js params
                              (.set (encode k) (encode v))))
                          (new URLSearchParams) query)]
    (-> (url (j/get js/location :href))
        (j/assoc! :search (str params))
        (str)
        (nav!))))

(defn swap-query!
  "Navigates to current route with query parameters modified by `f`,
   which is passed the current query-map followed by `args`."
  [f & args]
  (-> (apply f (query (get-route)) args)
      (query-nav!)))

(defn closest
  "Return element or first ancestor of element that matches predicate, like jQuery's .closest()."
  [^js el selector]
  {:pre [(string? selector)]}
  (if (.matches el selector)
    el
    (.closest el selector)))

(def ^:dynamic *click-event* nil)

(defn external? [^js link]
  (or (not= (.-host js/location) (.-host link))
      (not= (.-protocol js/location) (.-protocol link))
      (re-find #"\.[^/]+$" (.-pathname link))))

(defn valid-anchor? [^js link]
  (and (.-hash link)
       (= (.-pathname js/location) (.-pathname link))
       (not= (.-hash js/location) (.-hash link))
       (.getElementById js/document (subs (.-hash link) 1))))

(defn click-event-handler
  "Intercept clicks on links with valid pushstate hrefs. Callback is passed the link's href value."
  [callback ^js e]
  (when-let [link-element (closest (.-target e) "a")]
    (let [ignore-click? (or (external? link-element)
                            (valid-anchor? link-element)
                            (.-defaultPrevented e))]
      (when-not ignore-click?
        (.preventDefault e)
        (binding [*click-event* e]
          (let [{:keys [protocol
                        host]} (j/lookup js/location)
                ;; in IE/Edge, link elements do not expose an `origin` attribute
                origin (str protocol "//" host)]
            (callback (str/replace (j/get link-element :href) origin ""))))))))

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

(defn ^:dev/after-load fire!
  ([]
   (fire! (vals @listeners)))
  ([listener-fns]
   (let [route-state (-> (get-route)
                         (parse-path)
                         (cond-> *click-event*
                                 (assoc :click-event *click-event*)))]
     (doseq [listener listener-fns]
       (listener route-state)))))

(defonce history-init
  (delay
    (j/update! js/window "onpopstate"
               (fn [f]
                 (fn [event]
                   (when f (f event))
                   (fire!))))))

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

   (when intercept-clicks?
     (intercept-clicks))

   (swap! listeners assoc listener listener)

   (when fire-now?
     (fire! #{listener}))

   @history-init

   listener))

(defn unlisten [f]
  (swap! listeners dissoc f))

;;;;

(comment
  (assert (= (segments "/") []))
  (assert (= (segments "//") [""]))
  (assert (= (segments "///") ["" ""]))
  (assert (= (segments "/a/b")
             (segments "a/b/")
             (segments "a/b") ["a" "b"])))