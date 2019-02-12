(ns chia.view.hiccup.impl
  (:require [clojure.string :as str]
            ["react" :as react]
            [chia.util.js-interop :as j]
            [chia.util.perf :as perf]
            [chia.util :as u]))

(defn parse-key
  "Parses a hiccup key like :div#id.class1.class2 to return the tag name, id, and classes.
   If tag-name is ommitted, defaults to 'div'. Class names are padded with spaces."
  [x]
  (-> (re-find #":([^#.]*)(?:#([^.]+))?(.*)?" (str x))
      (update 1 #(if (= "" %) "div" (str/replace % "/" ":")))
      (update 3 #(when %
                   (str/replace (subs % 1) "." " ")))))

;; parse-key is an ideal target for memoization, because keyword forms are
;; frequently reused (eg. in lists) and almost never generated dynamically.
(def parse-key-memoized (memoize parse-key))

(defn reduce-flatten-seqs
  "Recursively apply f to nested vectors, unwrapping seqs. Similar to recursive `mapcat` but returns a vector."
  [f init conj-fn coll]
  (reduce (fn [c x]
            (if (seq? x)
              (reduce-flatten-seqs f c conj-fn x)
              (conj-fn c (f x)))) init coll))

(defn parse-args
  "Return props and children for a hiccup form. If the second element is not a map, supplies an empty map as props."
  [form]
  (let [len (count form)]
    (cond (= len 1) [{} []]
          (let [first-child (form 1)]
            (or (nil? first-child)
                (instance? PersistentArrayMap first-child)
                (instance? PersistentHashMap first-child)
                (and (= js/Object (.-constructor first-child))
                     (not (react/isValidElement first-child))))) [(form 1) (if (> len 2) (subvec form 2 len) [])]
          :else [{} (subvec form 1 len)])))

(defn update-attr [form attr f & args]
  (let [[props children] form]
    (into [(form 0)
           (assoc props attr (apply f (get form attr) args))]
          children)))

(defn key->react-attr
  "Return js (react) key for keyword/string.

  - Namespaced keywords are ignored
  - area- and data- prefixed keys are not camelCased
  - other keywords are camelCased"
  [k]
  (cond (string? k) k
        (namespace k) nil
        (perf/identical? :for k) "htmlFor"
        :else
        (let [s (name k)]
          (if (or (str/starts-with? s "data-")
                  (str/starts-with? s "aria-"))
            s
            (u/camel-case s)))))

(defn map->js
  "Return javascript object with camelCase keys. Not recursive."
  [style]
  (let [style-js (js-obj)]
    (doseq [[k v] style]
      (aset style-js (u/camel-case (name k)) v))
    style-js))

(defn merge-classes
  "Build className from keyword classes, :class and :classes."
  ;; TODO
  ;; benchmark different ways of merging strings.
  ;; eg. use a clojure StringBuilder,
  ;;     transient vs. ordinary vector
  [^string k-classes ^string class]
  (when (or k-classes class)
    (str k-classes
         (when (and k-classes class)
           " ")
         class)))

(def ^:dynamic *wrap-props* nil)

(defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  ([props] (props->js "" "" nil props))
  ([tag k-id k-classes props]
   (when (or props k-id k-classes)
     (if (map? props)
       (let [{:keys [class] :as props} (cond-> props
                                               (boolean *wrap-props*)
                                               (*wrap-props* tag))
             className (merge-classes k-classes class)
             prop-js (cond-> (js-obj)
                             k-id (j/assoc! :id k-id)
                             className (j/assoc! :className className))]
         (doseq [[k v] (dissoc props :class)]
           (when-let [js-key (key->react-attr k)]
             (unchecked-set prop-js js-key (cond-> v
                                                   (perf/keyword-in? [:style
                                                                      :dangerouslySetInnerHTML] k)
                                                   (map->js)))))
         prop-js)
       props))))

(defn js-conj [ar x]
  (doto ar
    (.push x)))

(defn clj->js-args! [js-args to-element]
  (let [props (aget js-args 1)
        props (cond-> props
                      (map? props) (props->js))]
    (reduce-flatten-seqs to-element
                         #js [(aget js-args 0) props]
                         js-conj
                         (.slice js-args 2))))


(def ^:dynamic *fragment* react/Fragment)