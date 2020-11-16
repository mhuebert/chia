(ns yawn.convert
  (:require #?@(:cljs [[yawn.react :as react]
                       [applied-science.js-interop :as j]]
                :clj  [[net.cgrand.macrovich :as m]])
            [clojure.string :as str]
            [yawn.env :as env]
            [yawn.util :as util]
            [yawn.emit-js :as emit-js]
            yawn.macros)
  #?(:cljs (:require-macros yawn.convert
                            [net.cgrand.macrovich :as m])))

(defn warn-on-interpret [options expr]
  (when-not (:interpet (meta expr))
    (when (:warn-on-interpretation? options)
      (println (str "WARNING: interpreting form " (pr-str expr)
                    (let [{:keys [line file]} (meta expr)]
                      (when (and line file)
                        (str ", " file ":" line))))))
    (when (:throw-on-interpretation? options)
      (throw (ex-info "Interpreting form" {:form expr})))))

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
    [options m]
    (if (map? m)
      (camel-case-keys m)
      (do
        (warn-on-interpret options m)
        `(camel-case-keys ~m)))))

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

  (defmacro join-strings-compile
    "Joins strings, space separated"
    [options-sym sep v]
    (cond (string? v) v
          (vector? v)
          (if (every? string? v)
            (str/join sep v)
            (m/case :cljs
                    (emit-js/join-strings sep v)
                    :clj
                    `(str/join ~sep ~(vec v))))
          :else
          (do
            (warn-on-interpret (resolve options-sym) v)
            `(~'yawn.compiler/maybe-interpret-class ~options-sym ~v)))))

(defn join-strings [options v]
  #?(:cljs
     (util/if-cljs-macrotime
       `(join-strings-compile ~(:js-options-sym options) " " ~v)
       (if (vector? v)
         (str/join " " v)
         v))

     :clj
     `(join-strings-compile ~(:js-options-sym options) " " ~v)))

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

(defn format-style-prop [options v]
  #?(:clj
     (if (vector? v)
       (mapv (partial camel-case-keys-compile options) v)
       (camel-case-keys-compile options v))

     :cljs
     (if (vector? v)
       (mapv camel-case-keys v)
       (camel-case-keys v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse props (top-level)

(defn add-prop [options prop-handlers assoc-prop m k v]
  {:pre [(or (map? prop-handlers)
             #?(:cljs (object? prop-handlers)))]}
  (let [kname (if (string? k)
                k (camel-case (name k)))]
    (if-some [handler #?(:cljs (util/if-cljs-macrotime
                                 (get prop-handlers kname)
                                 (j/!get prop-handlers kname))
                         :clj (get prop-handlers kname))]
      (handler options prop-handlers assoc-prop m kname v)
      (assoc-prop m kname v))))

(m/usetime
  #?(:cljs
     (defn js-set [o k v]
       (j/!set o ^string k v))))

(defn compile-props [options props]
  (let [handlers (get options :prop-handlers)]
    (reduce-kv (fn [m k v] (add-prop options handlers assoc m k v)) {} props)))

(defn convert-props [options props]
  #?(:cljs (util/if-cljs-macrotime
             (compile-props options props)
             (if
               (object? props)
               props
               (let [handlers (j/!get options :prop-handlers)]
                 (reduce-kv
                   (fn [m k v] (add-prop options handlers js-set m k v))
                   #js{}
                   props))))
     :clj  (compile-props options props)))

(m/deftime
  (env/set-defaults!
    '{;; settings for the compiler:
      :warn-on-interpretation? true
      :skip-types #{number
                    string
                    function
                    js}
      :rewrite-for? true

      ;; relevant for the interpreter:
      :custom-elements {"Fragment" yawn.react/Fragment
                        "<>" yawn.react/Fragment
                        "Suspense" yawn.react/Suspense
                        ">" "yawn/create-element"}
      :create-element yawn.react/createElement
      :create-element-compile [.createElement (yawn.react/get-react)]
      :prop-handlers
      {"class"
       (fn [options handlers assoc-prop m k v]
         (assoc-prop m "className" (yawn.convert/join-strings options v)))
       "for"
       (fn [options handlers assoc-prop m k v]
         (assoc-prop m "htmlFor" v))
       "style"
       (fn [options handlers assoc-prop m k v]
         (assoc-prop m k (yawn.convert/format-style-prop options v)))
       "&"
       (fn [options handlers assoc-prop m k v]
         (reduce-kv (fn [m k v] (yawn.convert/add-prop options handlers assoc-prop m k v)) m v))}}))

(env/def-options defaults {})

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
       ([options element-type form ^number prop-position]
        (let [props (-nth form prop-position js/undefined)
              props? (not (identical? js/undefined props))]
          (make-element options
                        element-type
                        (when props?
                          (convert-props options props))
                        form
                        (cond-> prop-position props? inc))))
       ([options element-type props-obj form children-start]
        (let [form-count (count form)]
          (case (- form-count children-start)               ;; fast cases for small numbers of children
            0 (.call react/createElement nil element-type props-obj)
            1 (.call react/createElement nil element-type props-obj (as-element options (nth form children-start)))
            2 (.call react/createElement nil element-type props-obj (as-element options (nth form children-start)) (as-element options (nth form (+ children-start 1))))
            3 (.call react/createElement nil element-type props-obj (as-element options (nth form children-start)) (as-element options (nth form (+ children-start 1))) (as-element options (nth form (+ children-start 2))))
            (let [out #js[element-type props-obj]]
              (loop [i children-start]
                (if (== i form-count)
                  (.apply react/createElement nil out)
                  (do
                    (.push out (as-element options (nth form i)))
                    (recur (inc i))))))))))

     (defprotocol IElement
       (-as-element [form]))

     (defn interpret-vec [options form]
       (let [form-0 (-nth form 0)]
         (if (keyword? form-0)
           (let [tag (j/get-in options [:custom-elements (name form-0)]
                               (if (keyword? form-0)
                                 (name form-0)
                                 form-0))]
             (if (identical? tag react/Fragment)
               (make-element options tag nil form 1)
               (j/let [create-element? (identical? tag "yawn/create-element")
                       tag (if create-element? (-nth form 1) tag)
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
                                     (->> (convert-props options))
                                     (string? tag)
                                     (add-static-props parsed-tag))
                       children-start (cond-> prop-position
                                              props? inc)]
                 (make-element options
                               tag
                               props
                               form
                               children-start))))
           (as-element options (apply form-0 (rest form))))))

     (defn as-element
       ([form]
        (as-element defaults form))
       ([options form]
        (cond (vector? form) (interpret-vec options form)
              (seq? form) (make-element options react/Fragment nil (vec form) 0)
              (satisfies? IElement form) (-as-element form)
              :else form)))))

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
