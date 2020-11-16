(ns yawn.compiler
  "
  Hicada - Hiccup compiler aus dem Allgaeu

  NOTE: The code for has been forked like this:
  weavejester/hiccup -> r0man/sablono -> Hicada."
  (:refer-clojure :exclude [compile])
  (:require
    cljs.analyzer
    [yawn.wrap-return :refer [wrap-return]]
    [yawn.convert :as convert]
    [yawn.infer :as infer]
    [yawn.util :as util]
    [yawn.convert :as convert]
    [yawn.emit-js :as to-js]))

(defn pass-options [opts]
  {:pre [(some? opts)]
   :post [(symbol? %)]}
  (cond-> opts (map? opts) :js-options-sym))

(defn children-as-list
  "Normalize the children of a HTML element."
  [x]
  (cond (nil? x) x
        (vector? x) (list x)
        (sequential? x) x
        :else (list x)))

(defn props-mode [props]
  (or (when (map? props) :map)
      (when (nil? props) :nil)
      (when (or (seq? props) (symbol? props))
        (let [props-meta (meta props)]
          (cond (:props props-meta) :dynamic
                (#{'object
                   'js} (:tag props-meta)) :js-object)))
      :no-props))

(defn analyze-vec
  "Given:
  [:div.x.y#id (other)]
  Returns:
  [:div {:id \"id\"
         :class [\"x\" \"y\"]}
    (other)]"
  [options [tag & body :as vec]]
  (let [[tag id class-string] (if (or (keyword? tag)
                                      (string? tag))
                                (convert/parse-tag (name tag))
                                [tag nil nil])
        tag-override (get-in options [:custom-elements tag])
        is-element? (= "yawn/create-element" tag-override)
        create-element? (or (some? tag-override)
                            is-element?
                            (string? tag)
                            (keyword? tag)
                            (= 'js (:tag (meta tag))))]
    (if create-element?
      (let [[tag body] (if is-element? [(first body) (rest body)]
                                       [(or tag-override tag) body])
            props (first body)
            mode (props-mode props)
            props? (not= mode :no-props)]
        [tag
         (when props? props)
         (children-as-list (cond-> body props? next))
         (merge
           {:create-element? true
            :id id
            :class-string class-string
            :prop-mode mode}
           (select-keys (meta vec) [:ref :key]))])
      [tag nil body {:form-meta (meta vec)}])))


(comment
  (analyze-vec [:div#foo 'a])
  (analyze-vec [:div.a#foo])
  (analyze-vec [:h1.b {:className "a"}])
  (analyze-vec '[:div (for [x xs] [:span 1])]))

(defn compile-mode
  [form]
  (if (util/primitive? form)
    :inline
    (let [{:keys [inline interpret tag]} (meta form)]
      (cond (= tag 'js) :inline
            inline :inline
            interpret :interpret
            (vector? form) :compile
            :else :maybe-interpret))))

(defmacro maybe-interpret-class [options-sym s]
  {:pre [(some? options-sym) (symbol? options-sym)]}
  (if (= 'string (infer/infer-type s &env))
    s
    (do
      (convert/warn-on-interpret (resolve options-sym) s)
      `(~'yawn.convert/class-string ~s))))

(defmacro create-element [options-sym & args]
  `(.call ~options-sym nil ~@args))

(declare emit)

(defn compile-vec
  "Returns an unevaluated form that returns a react element"
  [options [tag :as form]]
  (let [[tag props children form-opts] (analyze-vec options form)]
    (emit options tag props children form-opts)))

(defn compile-or-interpret-child
  "Compiles hiccup forms & wraps ambiguous forms for runtime interpretation"
  [options form]
  (if (map? form)
    (throw (ex-info "a map is an invalid child" {:form form}))
    (case (compile-mode form)
      :inline form
      :interpret (do (convert/warn-on-interpret options form)
                     `(~'yawn.convert/as-element ~(pass-options options) ~form))
      :compile (compile-vec options form)
      :maybe-interpret
      (or (wrap-return form (partial compile-or-interpret-child options) options)
          `(infer/maybe-interpret ~(pass-options options) ~form)))))

(defn compile-hiccup-child
  "Only compiles 'obvious' potential hiccup forms, ie. vectors... other args
   are untouched."
  [options form]
  (or (wrap-return form compile-hiccup-child options)
      (case (compile-mode form)
        :compile (compile-vec options form)
        :interpret `(~'yawn.convert/as-element ~(pass-options options) ~form)
        form)))

(defn emit
  "Emits the final react js code"
  [options tag props children {:as form-options
                               :keys [create-element?
                                      id
                                      class-string
                                      key
                                      ref
                                      prop-mode
                                      form-meta]}]
  (let [runtime-static-props (fn [form]
                               ;; adds dynamic props to js-props object at runtime
                               (if-let [ops (-> (for [[k v] {"id" id "key" key "ref" ref}
                                                      :when v]
                                                  `(~'applied-science.js-interop/!set ~k ~v))
                                                (cond-> class-string
                                                        (conj `(~'yawn.convert/update-class! ~class-string)))
                                                seq)]
                                 `(-> ~form ~@ops)
                                 form))]
    (if create-element?
      `(~@(:create-element-compile options)
         ~tag
         ~(case prop-mode
            ;; literal, we can add static props at compile-time
            (:map :nil :no-props)
            (as-> (or props {}) props*
                  (dissoc props* :&)
                  (into props* (filter val) {:id id :key key :ref ref})
                  (convert/compile-props options props*)
                  (cond-> props*
                          class-string (update "className" #(cond (nil? %) class-string
                                                                  (string? %) (str class-string " " %)
                                                                  :else `(str ~(str class-string " ") ~%))))
                  (to-js/literal->js props*)
                  (if (:& props)
                    `(-> ~props*
                         (~'applied-science.js-interop/extend!
                           (~'yawn.convert/convert-props ~(pass-options options) ~(:& props))))
                    props*))
            ;; dynamic clj, need to interpret & then add static props
            :dynamic
            (runtime-static-props
              (when props
                `(~'yawn.convert/convert-props ~(pass-options options) ~props)))
            ;; skip interpret, but add static props
            :js-object
            (runtime-static-props props))
         ~@(mapv (partial compile-or-interpret-child options) children))
      ;; clj-element
      `(~'yawn.infer/maybe-interpret ~(pass-options options) ~(with-meta `(~tag ~@(mapv (partial compile-hiccup-child (pass-options options)) children)) form-meta)))))

(defn compile
  "Arguments:
  - content: The hiccup to compile
  - opts
   o :warn-on-interpretation? - Print warnings when code cannot be pre-compiled and must be interpreted at runtime? (Defaults to `true`)
   o :inlineable-types - CLJS type tags that are safe to inline without interpretation. Defaults to `#{'number 'string}`
   o :is-hiccup? (opt) fn of expr that returns true (interpret), false (skip), or nil (maybe-interpret)
   o :create-element 'yawn.convert/createElement - you can also use your own function here.
   o :camelcase-key-pred - defaults to (some-fn keyword? symbol?), ie. map keys that have
                           string keys, are NOT by default converted from kebab-case to camelCase!
  - tag-handlers:
   A map to handle special tags. Run before compile."
  ([content]
   (compile convert/defaults content))
  ([options content]
   {:pre [(map? options)]}
   (compile-or-interpret-child options content)))

(defmacro as-element
  ([options-sym content]
   {:pre [(symbol? options-sym)]}
   (compile @(resolve options-sym) content))
  ([content]
   (compile convert/defaults content)))

