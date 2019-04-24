(ns chia.view.hiccup.impl
  (:require [clojure.string :as str]
            ["react" :as react]
            [applied-science.js-interop :as j]
            [chia.util.perf :as perf]
            [chia.util :as u]))

(defn parse-key
  "Parses a hiccup key like :div#id.class1.class2 to return the tag name, id, and classes.
   If tag-name is ommitted, defaults to 'div'. Class names are padded with spaces."
  [x]
  (let [[_ tag id classes] (re-find #"([^#.]*)(?:#([^.]+))?(?:\.(.*))?" (name x))]
    [(if (identical? tag "") "div" tag)
     id
     (some-> classes (str/replace "." " "))]))

(defonce parse-key-memoized (u/memoize-1 parse-key))

(defn reduce-flatten-seqs
  "Recursively apply f to nested vectors, unwrapping seqs. Similar to recursive `mapcat` but returns a vector."
  [f init conj-fn coll]
  (->> coll
       (reduce (fn [c x]
                 (if (seq? x)
                   (reduce-flatten-seqs f c conj-fn x)
                   (conj-fn c (f x)))) init)))

(defn split-args
  "Return props and children for a hiccup form. If the second element is not a map, supplies an empty map as props."
  [form]
  (let [len (count form)]
    (cond (identical? len 1) form
          (let [first-child (nth form 1)]
            (or (nil? first-child)
                (map? first-child))) (conj (subvec form 0 2) (subvec form 2))
          :else (conj [(nth form 0) nil] (subvec form 1)))))

(defn key->react-attr
  "Return js (react) key for keyword/string.

  - Namespaced keywords are ignored
  - area- and data- prefixed keys are not camelCased
  - other keywords are camelCased"
  [k]
  (cond (string? k) k
        (.-ns k) nil
        (perf/identical? :for k) "htmlFor"
        :else
        (let [s (name k)]
          (if ^boolean (or (str/starts-with? s "data-")
                           (str/starts-with? s "aria-"))
            s
            (u/camel-case s)))))

(defn styles->js
  "Return javascript object with camelCase keys. Not recursive."
  [style]
  (let [obj (js-obj)]
    (doseq [[k v] style]
      (aset obj (u/camel-case (name k)) v))
    obj))

(def ^:dynamic *wrap-props* nil)

(defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  ([props] (props->js nil nil nil props))
  ([tag k-id k-classes props]
   (let [props? (map? props)
         {:keys [class] :as props} (when props?
                                     (cond-> props *wrap-props* (*wrap-props* tag)))
         className (cond-> k-classes
                           class (str (when k-classes " ") class))
         prop-js (cond-> (js-obj)
                         k-id (j/assoc! :id k-id)
                         className (j/assoc! :className className))]
     (when props?
       (doseq [[k v] (dissoc props :class)]
         (when-some [js-key (key->react-attr k)]
           (unchecked-set prop-js js-key (cond-> v
                                                 (perf/keyword-in? [:style
                                                                    :dangerouslySetInnerHTML] k)
                                                 (styles->js))))))
     prop-js)))

(defn clj->js-args! [js-args to-element]
  (let [props (aget js-args 1)
        props (if (map? props)
                (props->js props)
                (to-element props))]
    (->> (.slice js-args 2)
         (reduce (fn [out x] (j/push! out (to-element x)))
                 #js [(aget js-args 0) props]))))

#_(defn update-attr [form attr f & args]
    (let [[props children] form]
      (into [(form 0)
             (assoc props attr (apply f (get form attr) args))]
            children)))