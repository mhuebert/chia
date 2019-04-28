(ns chia.view.hiccup.impl
  (:require [clojure.string :as str]
            ["react" :as react]
            [applied-science.js-interop :as j]
            [chia.util.perf :as perf]
            [chia.util :as u]
            [goog.object :as gobj]))

(defn parse-key*
  "Parses a hiccup key like :div#id.class1.class2 to return the tag name, id, and classes.
   If tag-name is ommitted, defaults to 'div'. Class names are padded with spaces."
  [x]
  (let [match (.exec #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?" x)]
    (j/obj .-tag (or (aget match 1) "div")
           .-id (aget match 2)
           .-classes (some-> (aget match 3) (str/replace "." " ")))))

(def parse-key (u/memoize-str parse-key*))

(defn reduce-flatten-seqs
  "Recursively apply f to nested vectors, unwrapping seqs. Similar to recursive `mapcat` but returns a vector."
  [f init conj-fn coll]
  (->> coll
       (reduce (fn [c x]
                 (if (seq? x)
                   (reduce-flatten-seqs f c conj-fn x)
                   (conj-fn c (f x)))) init)))

(defn name->react-attr*
  "Return js (react) key for keyword/string.

  - Namespaced keywords are ignored
  - area- and data- prefixed keys are not camelCased
  - other keywords are camelCased"
  [s]
  (cond (identical? s "for") "htmlFor"
        (identical? s "class") "className"
        (or (str/starts-with? s "data-")
            (str/starts-with? s "aria-")) s
        :else (u/camel-case s)))

(def name->react-attr (u/memoize-str name->react-attr*))

(defn map->js
  "Return javascript object with camelCase keys (shallow)"
  [style]
  (->> style
       (reduce-kv (fn [obj k v]
                    (j/assoc! obj (u/camel-case (name k)) v)) #js{})))

(def ^:dynamic *wrap-props* nil)

(defn- map-prop? [js-key]
  (or (identical? js-key "style")
      (identical? js-key "dangerouslySetInnerHTML")))

(defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  ([props] (props->js #js{} props))
  ([parsed-key props]
   (->> (cond-> (or props {})
                (some? *wrap-props*) (*wrap-props* (.-tag parsed-key))
                (.-id parsed-key) (assoc :id (.-id parsed-key))
                (.-classes parsed-key) (update :class (fn [x]
                                                        (if (some? x)
                                                          (str (.-classes parsed-key) " " x)
                                                          (.-classes parsed-key)))))
        (reduce-kv
         (fn [js-props k v]
           (if-some [js-key (when-not (qualified-keyword? k)
                              (name->react-attr (name k)))]
             (j/unchecked-set js-props js-key
                              (cond-> v (map-prop? js-key) (map->js)))
             js-props)) (j/obj)))))
