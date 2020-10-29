(ns hicada.util
  (:require
    #?(:cljs
       [applied-science.js-interop :as j])
    [clojure.string :as str]
    [clojure.set :refer [rename-keys]]))

(defn guard [x f] (when (f x) x))

(defn map-vals [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

(defn interpreted? [x]
  (some-> (guard x seq?)
          first
          (guard symbol?)
          namespace
          (= "hicada.interpreter")))

(defn join-classes-js-emit
  "Joins strings space separated"
  ([] "")
  ([& xs]
   (let [strs (->> (repeat (count xs) "~{}")
                   (interpose ",")
                   (apply str))]
     (list* 'js* (str "[" strs "].join(' ')") xs))))

(defn camel-case [s]
  (cond-> s
          (not (or (str/starts-with? s "data-")
                   (str/starts-with? s "aria-")))
          (str/replace #"-(.)" (fn [[_ s]] (str/upper-case s)))))

#?(:cljs
   (defn camel-case-keys-js [m]
     (reduce-kv
       (fn [m k v]
         (j/!set m (camel-case (name k)) v))
       #js{} m)))

(defn camel-case-keys-emit [m]
  `(~'hicada.util/camel-case-keys-js ~m))

(defn camel-case-keys
  "returns map with keys camel-cased"
  [m]
  (if (map? m)
    (reduce-kv
      (fn [m k v]
        (assoc m (camel-case (name k)) v))
      {} m)
    (camel-case-keys-emit m)))

(defn html-to-dom-attrs
  "Converts all HTML attributes to their DOM equivalents."
  [attrs]
  (rename-keys (camel-case-keys attrs)
               {"class" "className"
                "for" "htmlFor"}))

(def ^:private dot-pattern #?(:cljs (js/RegExp "\\." "g")
                              :clj  "."))

(defn replace-pattern [s pattern rep]
  #?(:clj (str/replace s pattern rep)
     :cljs (.replace s (j/!set pattern :lastIndex 0) rep)))

(defn dots->spaces [s]
  (replace-pattern s dot-pattern " "))

(defn parse-tag
  "Returns array of [tag-name, id, classes] from a tag-name like div#id.class1.class2"
  [tag-name]
  (let [pattern #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?"]
    #?(:cljs (-> (.exec pattern tag-name)
                 (.slice 1 4)
                 (j/update! 2 #(when % (dots->spaces %))))
       :clj  (-> (rest (re-find pattern tag-name))
                 vec
                 (update 2 #(when % (dots->spaces %)))))))

(defn hiccup-vec? [form]
  (and (vector? form) ((some-fn keyword? symbol?) (first form))))

(defn compile-prop [xf m k v]
  (let [kname (if (string? k)
                k (camel-case (name k)))]
    (case kname "class"
                (xf m "className"
                    (cond (string? v) v
                          (vector? v)
                          (if (every? string? v)
                            (str/join " " v)
                            (join-classes-js-emit v))
                          :else `(~'hicada.compiler/ensure-class-string ~v)))
                "for"
                (xf m "htmlFor" v)
                "style"
                (xf m kname (if (vector? v)
                              (mapv camel-case-keys v)
                              (camel-case-keys v)))
                (xf m kname v))))


(defn interpret-prop [xf m k v]
  (let [kname (camel-case k)]
    (case kname "class"
                (xf m "className"
                    (if (vector? v)
                      (str/join " " v)
                      v))
                "for"
                (xf m "htmlFor" v)
                "style"
                (xf m kname #?(:cljs (camel-case-keys-js v)
                               :clj (camel-case-keys v)))
                v)))

(defn assoc-some
  ([m k v] (cond-> m (some? v) (assoc k v)))
  ([m k v & kvs]
   (let [ret (assoc-some m k v)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

#?(:cljs
   (defn assoc-some-js!
         ([m k v] (cond-> m (some? v) (j/assoc! k v)))
         ([m k v & kvs]
          (let [ret (assoc-some-js! m k v)]
               (if kvs
                 (recur ret (first kvs) (second kvs) (nnext kvs))
                 ret)))))
