(ns chia.view.hiccup.impl
  (:require [clojure.string :as str]
            ["react" :as react]
            [applied-science.js-interop :as j]
            [chia.util :as u]
            [chia.view.render-loop :as render-loop]))

(defn- update-change-prop [props]
  (if-some [on-change (get props :on-change)]
    (assoc props :on-change (render-loop/apply-sync! on-change))
    props))

(defn parse-key
  "Parses a hiccup key like :div#id.class1.class2 to return the tag name, id, and classes.
   If tag-name is ommitted, defaults to 'div'. Class names are padded with spaces."
  [x]
  (let [match (.exec #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?" x)
        classes (aget match 3)]
    #js[;; tag
        (or (aget match 1) "div")
        ;; id
        (aget match 2)
        ;; classes
        (if (undefined? classes)
          classes
          (str/replace (aget match 3) "." " "))]))

(def parse-key-memo (u/memoize-str parse-key))

(defn react-prop-name
  "Return js (react) key for keyword/string.

  - area- and data- prefixed keys are not camelCased
  - other keywords are camelCased"
  [s]
  (cond (identical? s "for") "htmlFor"
        (identical? s "class") "className"
        (or (str/starts-with? s "data-")
            (str/starts-with? s "aria-")) s
        :else (u/camel-case s)))

(def prop-name-memo (u/memoize-str react-prop-name))

(defn map->js
  "Return javascript object with camelCase keys (shallow)"
  [style]
  (->> style
       (reduce-kv (fn [obj k v]
                    (j/assoc! obj (u/camel-case (name k)) v)) #js{})))

(def prop-update-fns #js{:onChange render-loop/apply-sync!
                         :style map->js
                         :dangerouslySetInnerHTML map->js})

(defn- defined? [x] (not (undefined? x)))

(defn prop-val [prop-name val]
  (let [update-fn (j/!get prop-update-fns ^string prop-name)]
    (if (defined? update-fn)
      (update-fn val)
      val)))

(defn class-str [s]
  (cond (string? s) s
        (vector? s) (str/replace (str/join " " (mapv class-str s))
                                 "." " ")
        (keyword? s) (name s)))

(defn set-prop! [js-props k v]
  (if (qualified-keyword? k)
    js-props
    (let [prop-name (prop-name-memo (name k))]
      (j/!set js-props prop-name (prop-val prop-name v)))))

(defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  ([props] (props->js js/undefined js/undefined js/undefined props))
  ([tag id classes props]
   (as-> (if (some? props) props {}) props
         (cond-> props (defined? id) (assoc :id id))
         (let [props-classes (get props :class)]
           (cond (some? props-classes)
                 (assoc props :class (cond->> (class-str props-classes)
                                              (defined? classes)
                                              (str classes " ")))
                 (defined? classes)
                 (assoc props :class classes)
                 :else props))
         (reduce-kv set-prop! #js{} props))))
