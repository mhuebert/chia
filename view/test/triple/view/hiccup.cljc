(ns triple.view.hiccup
  (:require ["react" :as react]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [triple.util.memo :as memo]
            [triple.util.string :as string]
            [triple.util.cond :as cond]
            [triple.view.props :as props])
  (:require-macros triple.view.hiccup))


;; because of CLJS bug https://clojure.atlassian.net/browse/CLJS-2380
;; we use a marker key instead of a protocol
(def ^:private IClojureView (munge `IClojureView))

(defn mark-clj-view! [x]
  (j/!set x ^string IClojureView true))

(defn is-clj-view? [x]
  (and (identical? "function" (js* "typeof ~{}" x))
       ^boolean (js-in IClojureView x)))

;; Props and children are handled in 2 ways:
;;
;; 1. IClojureView (defined by triple.view)
;;    - pass both props & children as arguments to the function
;;    - extract react `key` and `ref` from props
;;
;; 2. All other views (eg. :div, SomeJSView)
;; - convert props to JS, child vectors to hiccup

(defn map->js-camel
  "Return javascript object with camelCase keys (shallow)"
  [v]
  (if (map? v)
    (->> v
         (reduce-kv (fn [obj k v]
                      (j/!set obj (string/camel-case (name k)) v)) (j/obj)))
    v))

(defn prop-map->js [o k v]
  (j/!set o k (cond-> v (map? v) (map->js-camel))))

(def ^:private prop-camel
  "Converts string attribute to camelCase unless prefixed by data- or aria-"
  (memo/by-string
    (fn [k]
      (cond-> k
              (not (or (str/starts-with? k "data-")
                       (str/starts-with? k "aria-")))
              (string/-camel-case)))))

(def ^:private dot-pattern #?(:cljs (js/RegExp "\\." "g")
                              :clj  "."))

(defn dots->spaces [s]
  (string/replace-pattern s dot-pattern " "))

(defn join-classes
  "Handles class strings and vectors"
  [s]
  (if (vector? s)
    (str/join " " (mapv join-classes s))
    s))

(def ^:private prop-handlers
  (j/obj
    :for (fn [o k v] (j/!set o "htmlFor" v))
    :class (fn [o _ v] (j/!set o "className" (join-classes v)))
    :style prop-map->js
    :dangerouslySetInnerHTML prop-map->js
    :default (fn [o k v] (j/!set o (prop-camel k) v))))

(defn set-prop-handler!
  "Register a custom prop handler. prop-name should be a string,
   handler a function of [object, name, value] and is responsible
   for returning a mutated object.

   Special cases:

   class     => value is only from clj :class, merges result with :className
   classname => clj :class merged with the tag's class (eg. :div.my-class)
   "
  [prop-name handler]
  (j/assoc! prop-handlers prop-name handler))

(defn- prop->js [o k v]
  (if (qualified-keyword? k)
    o
    (let [attr-name (name k)
          handler (cond/if-defined [handler (j/!get prop-handlers attr-name)]
                    handler
                    (j/!get prop-handlers "default"))]
      (handler o attr-name v))))

(defn- join-some-strs [a b]
  ;; joins `a` and `b` which may be nil
  (if (some? a)
    (if (some? b)
      (str a " " b)
      a)
    b))

(defn- react-props
  ;; extracts 'special' React props from clj map
  [props-map]
  (let [key (get props-map :key)
        ref (get props-map :ref)]
    (cond-> (j/obj)
            (some? key) (j/!set :key key)
            (some? ref) (j/!set :ref ref))))

(defn- js-reduce-kv
  ;; reduce-kv for a js object
  [f init obj]
  (let [obj-keys (js-keys obj)]
    (areduce obj-keys i
             out init
             (let [k (aget obj-keys i)]
               (f out k (j/!get obj ^string k))))))

(defn- js-props
  ;; converts clj map into js props
  [match props]
  (j/let [^:js [tag id classes] match
          js-props (if (object? props)
                     (js-reduce-kv prop->js #js{} props)
                     (reduce-kv prop->js #js{} props))
          id (j/!get js-props :id id)
          className (join-some-strs (j/!get js-props :className) classes)]
    (cond-> js-props
            (some? id)
            (prop->js "id" id)

            (some? className)
            (prop->js "className" (dots->spaces className)))))

(defn -parse-tag
  "Returns array of [tag-name, id, classes] from a tag-name like div#id.class1.class2"
  [tag-name]
  (let [pattern #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?"]
    #?(:cljs (-> (.exec pattern tag-name) (.slice 1 4))
       :clj  (rest (re-find pattern tag-name)))))

(def parse-tag (memo/by-string -parse-tag))

(defprotocol IElement
  (-to-element [this] "Returns a React element representing `this`"))

(declare to-element)

(defn make-element
  "Returns a React element. `tag` may be a string or a React component (a class or a function).
   Children will be read from `form` beginning at index `start`."
  ([element-type form]
   (let [props (props/get-props form 1)
         props? (cond/defined? props)
         clj? (is-clj-view? element-type)]
     (make-element element-type
                   (when props?
                     (if clj? (react-props props)
                              (js-props nil props)))
                   form
                   (if (or clj? (not props?)) 1 2)
                   clj?)))
  ([element-type props-obj form children-start clj?]
   (let [form-count (count form)
         to-element (if clj? identity to-element)]
     (case (- form-count children-start)                    ;; fast cases for small numbers of children
       0 (react/createElement element-type props-obj)
       1 (let [first-child (nth form children-start)]
           (react/createElement element-type props-obj (to-element first-child)))
       (let [out #js[element-type props-obj]]
         (loop [i children-start]
           (if (== i form-count)
             (.apply react/createElement nil out)
             (do
               (.push out (to-element (nth form i)))
               (recur (inc i))))))))))

(defonce tag-handlers
         (j/obj "#" react/Suspense
                "<>" react/Fragment))

(defn to-element
  "Converts Hiccup form into a React element"
  [form]
  (cond (vector? form) (let [tag (-nth form 0)]
                         (if (keyword? tag)
                           (j/let [^:js [tag-name :as match] (parse-tag (name tag))
                                   tag (j/!get tag-handlers
                                               tag-name
                                               tag-name)
                                   props (props/get-props form 1)
                                   props? (cond/defined? props)]
                             (make-element tag
                                           (js-props match (when props? props))
                                           form
                                           (if props? 2 1)
                                           false))
                           (make-element tag form)))
        (seq? form) (make-element react/Fragment nil (vec form) 0 false)
        (satisfies? IElement form) (-to-element form)
        :else form))

