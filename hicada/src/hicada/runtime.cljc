(ns hicada.runtime
  (:require #?@(:cljs
                [[applied-science.js-interop :as j]
                 ["react" :as react]
                 ["react-dom" :as react-dom]])
            [clojure.string :as str]
            [clojure.string :refer [blank? join]]
            [hicada.util :as util]
            [hicada.macros]
            #?(:clj [net.cgrand.macrovich :as m]))
  #?(:cljs (:require-macros hicada.runtime
                            [net.cgrand.macrovich :as m])))

#?(:cljs
   (do
     (def Fragment react/Fragment)
     (def Suspense react/Suspense)
     (def createElement react/createElement)))

(defn #?(:cljs ^string class-string
         :clj  class-string) [classes]
  (cond (string? classes) classes
        (vector? classes) (str/join " " classes)
        :else classes))

(defn update-class! [obj class-string]
  (let [f (fn [x]
            (if (some? x)
              (str class-string " " x)
              class-string))]
    #?(:cljs (j/update! obj :className f)
       :clj  (update obj :className f))))

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

  (defn join-classes-compile
    "Joins strings space separated"
    [v]
    (m/case :cljs
            (let [strs (->> (repeat (count v) "~{}")
                            (interpose ",")
                            (apply str))]
              (list* 'js* (str "[" strs "].join(' ')") v))
            :clj
            `(str/join " " ~(vec v))))

  (defn camel-case-keys-compile
    "returns map with keys camel-cased"
    [m]
    (if (map? m)
      (camel-case-keys m)
      `(~'hicada.runtime/camel-case-keys ~m))))

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

(m/usetime
  (defn format-class-prop [v]
    (if (vector? v)
      (str/join " " v)
      v)))

(m/deftime
  (defn format-class-prop [v]
    (cond (string? v) v
          (vector? v)
          (if (every? string? v)
            (str/join " " v)
            (join-classes-compile v))
          :else `(~'hicada.compiler/ensure-class-string ~v))))

(m/deftime
  (defn format-style-prop [v]
    (if (vector? v)
      (mapv camel-case-keys-compile v)
      (camel-case-keys-compile v))))

(m/usetime
  (defn format-style-prop [v]
    (camel-case-keys v)))

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

(m/deftime
  (defn convert-props [props]
    (reduce-kv (fn [m k v] (add-prop assoc m k v)) {} props)))

(m/usetime

  #?(:cljs
     (defn js-set [o k v]
       (j/!set o ^string k v)))

  (defn convert-props [props]
    #?(:cljs (if (object? props)
               props
               (reduce-kv
                 (fn [m k v] (add-prop js-set m k v))
                 #js{}
                 props))
       :clj  (reduce-kv (fn [m k v] (add-prop assoc m k v)) {} props))))

#?(:cljs
   (def tag-registry #js{"<>" Fragment
                         "Fragment" Fragment
                         "#" Suspense
                         "Suspense" Suspense
                         ">" "create-element"}))

(declare to-element)

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
              coerce (if interpret-children? to-element identity)]
          (case (- form-count children-start)               ;; fast cases for small numbers of children
            0 (.call createElement nil element-type props-obj)
            1 (.call createElement nil element-type props-obj (coerce (nth form children-start)))
            2 (.call createElement nil element-type props-obj (coerce (nth form children-start)) (coerce (nth form (+ children-start 1))))
            3 (.call createElement nil element-type props-obj (coerce (nth form children-start)) (coerce (nth form (+ children-start 1))) (coerce (nth form (+ children-start 2))))
            (let [out #js[element-type props-obj]]
              (loop [i children-start]
                (if (== i form-count)
                  (.apply createElement nil out)
                  (do
                    (.push out (coerce (nth form i)))
                    (recur (inc i))))))))))



     (defprotocol IElement
       (-to-element [form]))

     (defn interpret-vec [form]
       (let [form-0 (-nth form 0)]
         (if (keyword? form-0)
           (let [tag (j/!get tag-registry (name form-0)
                             (if (keyword? form-0)
                               (name form-0)
                               form-0))]
             (if (identical? tag Fragment)
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
           (to-element (apply form-0 (rest form))))))

     (defn to-element [form]
       (cond (vector? form) (interpret-vec form)
             (seq? form) (make-element Fragment nil (vec form) 0 true)
             ;(satisfies? IElement form) (-to-element form)
             :else form))))

(comment
  (to-element [:div])
  (to-element [:div.a])
  (to-element [:div.a {:class "b"}])
  (to-element [:div.a {:class ["b" "c"]}])
  (to-element [:div "a"])
  #?(:cljs (to-element [:div #js{} "a"]))

  (defn my-fn [x] [:div x])
  (to-element [my-fn "a"])
  (to-element [my-fn [:div.x]])

  (to-element [:div nil "a"])

  (to-element [:<> "a" "b"])
  (to-element [:<> [:div]])

  )
