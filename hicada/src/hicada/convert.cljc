(ns hicada.convert
  (:require #?@(:cljs  [[hicada.react :as react]
                        [applied-science.js-interop :as j]]
                :clj   [[net.cgrand.macrovich :as m]])
            [clojure.string :as str]
            [clojure.string :refer [blank? join]]
            [hicada.env :as env]
            [hicada.util :as util]
            [hicada.macros])
  #?(:cljs (:require-macros hicada.convert
                            [net.cgrand.macrovich :as m])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key transformation

(def camel-case
  (util/memo-by-string
    (fn [s]
      (cond-> s
              (not (or (str/starts-with? s "data-")
                       (str/starts-with? s "aria-")))
              (str/replace #"-(.)" (fn [[_ s]] (str/upper-case s)))))))

(defn camel-case-keys [m]
  #?(:cljs (reduce-kv
             (fn [m k v]
               (j/!set m (camel-case (name k)) v))
             #js{} m)
     :clj  (reduce-kv
             (fn [m k v]
               (assoc m (camel-case (name k)) v))
             {} m)))

(m/deftime
  (defn camel-case-keys-compile
    "returns map with keys camel-cased"
    [m]
    (if (map? m)
      (camel-case-keys m)
      `(camel-case-keys ~m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class string transformation

(defn #?(:cljs ^string class-string
         :clj  class-string) [classes]
  (cond (string? classes) classes
        (vector? classes) (str/join " " classes)
        :else classes))

(defn update-class! [obj class-str]
  (let [f (fn [x]
            (if (some? x)
              (str class-str " " x)
              class-str))]
    #?(:cljs (j/update! obj :className f)
       :clj  (update obj :className f))))

(m/deftime

  (defn class-vector-string-compile
    "Joins strings, space separated"
    [v]
    (m/case :cljs
            (let [strs (->> (repeat (count v) "~{}")
                            (interpose ",")
                            (apply str))]
              (list* 'js* (str "[" strs "].join(' ')") v))
            :clj
            `(str/join " " ~(vec v)))))

(defn format-class-prop [v]
  (util/casetime
    :usetime
    (if (vector? v)
      (str/join " " v)
      v)

    :deftime
    (cond (string? v) v
          (vector? v)
          (if (every? string? v)
            (str/join " " v)
            (class-vector-string-compile v))
          :else `(~'hicada.compiler/maybe-interpret-class ~v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse static tag names

(def ^:private dot-pattern #?(:cljs (js/RegExp "\\." "g")
                              :clj  "."))

(defn replace-pattern [s pattern rep]
  #?(:clj  (str/replace s pattern rep)
     :cljs (.replace s (j/!set pattern :lastIndex 0) rep)))

(defn dots->spaces [s]
  (replace-pattern s dot-pattern " "))

(def parse-tag
  "Returns array of [tag-name, id, classes] from a tag-name like div#id.class1.class2"
  (util/memo-by-string
    (fn [tag-name]
      (let [pattern #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?"]
        #?(:cljs (-> (.exec pattern tag-name)
                     (.slice 1 4)
                     (j/update! 2 #(if % (dots->spaces %) %)))
           :clj  (-> (rest (re-find pattern tag-name))
                     vec
                     (update 2 #(when % (dots->spaces %)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse style props

(defn format-style-prop [v]
  (util/casetime
    :deftime
    (if (vector? v)
      (mapv camel-case-keys-compile v)
      (camel-case-keys-compile v))

    :usetime
    (if (vector? v)
      (mapv camel-case-keys v)
      (camel-case-keys v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse props (top-level)

(defn add-prop [xf m k v]
  (let [kname (if (string? k)
                k (camel-case (name k)))]
    (case kname "class"
                (xf m "className" (format-class-prop v))
                "for"
                (xf m "htmlFor" v)
                "style"
                (xf m kname (format-style-prop v))
                (xf m kname v))))

(m/usetime
  #?(:cljs
     (defn js-set [o k v]
       (j/!set o ^string k v))))

(defn convert-props [props]
  (util/casetime
    :usetime
    #?(:cljs (if (object? props)
               props
               (reduce-kv
                 (fn [m k v] (add-prop js-set m k v))
                 #js{}
                 props))
       :clj  (reduce-kv (fn [m k v] (add-prop assoc m k v)) {} props))
    :deftime
    (reduce-kv (fn [m k v] (add-prop assoc m k v)) {} props)))

#?(:cljs
   (def tag-registry #js{"<>" react/Fragment
                         "Fragment" react/Fragment
                         "#" react/Suspense
                         "Suspense" react/Suspense
                         ">" "create-element"}))

(declare as-element)

#?(:cljs
   (m/usetime

     (defn defined? [x]
       (not (undefined? x)))

     (defn get-props [form i]
       (let [result (-nth form i js/undefined)]
         (if (undefined? result)
           result
           (if (or (object? result)
                   (map? result))
             result
             js/undefined))))

     (j/defn add-static-props [props ^:js [tag-name id class-string]]
       (cond-> props
               (defined? class-string) (update-class! class-string)
               (defined? id) (j/!set :id id)))

     (defn make-element
       "Returns a React element. `tag` may be a string or a React component (a class or a function).
        Children will be read from `form` beginning at index `start`."
       ([element-type form ^number prop-position]
        (let [props (-nth form prop-position js/undefined)
              props? (not (identical? js/undefined props))]
          (make-element element-type
                        (when props?
                          (convert-props props))
                        form
                        (cond-> prop-position props? inc)
                        true)))
       ([element-type props-obj form children-start interpret-children?]
        (let [form-count (count form)
              coerce (if interpret-children? as-element identity)]
          (case (- form-count children-start)               ;; fast cases for small numbers of children
            0 (.call react/createElement nil element-type props-obj)
            1 (.call react/createElement nil element-type props-obj (coerce (nth form children-start)))
            2 (.call react/createElement nil element-type props-obj (coerce (nth form children-start)) (coerce (nth form (+ children-start 1))))
            3 (.call react/createElement nil element-type props-obj (coerce (nth form children-start)) (coerce (nth form (+ children-start 1))) (coerce (nth form (+ children-start 2))))
            (let [out #js[element-type props-obj]]
              (loop [i children-start]
                (if (== i form-count)
                  (.apply react/createElement nil out)
                  (do
                    (.push out (coerce (nth form i)))
                    (recur (inc i))))))))))



     (defprotocol IElement
       (-as-element [form]))

     (defn interpret-vec [form]
       (let [form-0 (-nth form 0)]
         (if (keyword? form-0)
           (let [tag (j/!get tag-registry (name form-0)
                             (if (keyword? form-0)
                               (name form-0)
                               form-0))]
             (if (identical? tag react/Fragment)
               (make-element tag nil form 1 true)
               (j/let [create-element? (identical? tag "create-element")
                       prop-position (if create-element? 2 1)
                       props (get-props form prop-position)
                       props? (defined? props)
                       static-tag? (string? tag)
                       parsed-tag (when static-tag?
                                    (parse-tag tag))
                       tag (if static-tag?
                             (aget parsed-tag 0)
                             tag)
                       props (cond-> props
                                     props?
                                     (convert-props)
                                     (string? tag)
                                     (add-static-props parsed-tag))
                       children-start (cond-> prop-position
                                              props? inc)]
                 (make-element tag
                               props
                               form
                               children-start
                               true))))
           (as-element (apply form-0 (rest form))))))

     (defn as-element [form]
       (cond (vector? form) (interpret-vec form)
             (seq? form) (make-element react/Fragment nil (vec form) 0 true)
             ;(satisfies? IElement form) (-as-element form)
             :else form))))

(comment
  (as-element [:div])
  (as-element [:div.a])
  (as-element [:div.a {:class "b"}])
  (as-element [:div.a {:class ["b" "c"]}])
  (as-element [:div "a"])
  #?(:cljs (as-element [:div #js{} "a"]))

  (defn my-fn [x] [:div x])
  (as-element [my-fn "a"])
  (as-element [my-fn [:div.x]])

  (as-element [:div nil "a"])

  (as-element [:<> "a" "b"])
  (as-element [:<> [:div]])

  )
