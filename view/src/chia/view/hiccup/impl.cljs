(ns chia.view.hiccup.impl
  (:require [clojure.string :as str]
            ["react" :as react]))

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

(def camelCase
  (memoize (fn [s]
             (if (or (str/starts-with? s "data-")
                     (str/starts-with? s "aria-"))
               s
               (str/replace s #"-([a-z])" (fn [[_ s]] (str/upper-case s)))))))

(defn key->react-attr
  "CamelCase react keys.

  Exceptions:
   - aria- and data- attributes
   - namespaced keywords (:custom/attr => 'custom-attr')"
  [k]
  (cond (string? k) k
        (keyword-identical? k :for)
        "htmlFor"
        (namespace k) (str (namespace k) "-" (name k))
        :else
        (camelCase (name k))))

(defn map->js
  "Return javascript object with camelCase keys. Not recursive."
  [style]
  (let [style-js (js-obj)]
    (doseq [[k v] style]
      (aset style-js (camelCase (name k)) v))
    style-js))

(defn merge-classes
  "Build className from keyword classes, :class and :classes."
  ;; TODO
  ;; benchmark different ways of merging strings.
  ;; eg. use a clojure StringBuilder,
  ;;     transient vs. ordinary vector
  [^js/String k-classes ^js/String class classes]
  (cond-> []
          k-classes (conj k-classes)
          class (conj class)
          classes (into classes)
          true (->> (str/join " "))))

(def ^:dynamic *wrap-props* nil)

(defn props->js
  "Returns a React-conformant javascript object. An alternative to clj->js,
  allowing for key renaming without an extra loop through every prop map."
  ([props] (props->js "" "" nil props))
  ([tag k-id k-classes props]
   (when (or props k-id k-classes)
     (let [{:keys [class class-name classes] :as props} (cond-> props
                                                                (boolean *wrap-props*)
                                                                (*wrap-props* tag))
           prop-js (cond-> (js-obj)
                           k-id (doto (aset "id" k-id))
                           (or k-classes class class-name classes) (doto (aset "className" (merge-classes k-classes (or class class-name) classes))))]
       (doseq [[k v] props]
         (cond
           ;; convert :style and :dangerouslySetInnerHTML to js objects
           (or (keyword-identical? k :style)
               (keyword-identical? k :dangerouslySetInnerHTML))
           (aset prop-js (name k) (map->js v))
           ;; ignore className-related keys
           (or (keyword-identical? k :classes)
               (keyword-identical? k :class)) nil
           ;; passthrough all other values
           :else (aset prop-js (key->react-attr k) v)))
       prop-js))))

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