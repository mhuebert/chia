(ns hicada.compiler
  "
  Hicada - Hiccup compiler aus dem Allgaeu

  NOTE: The code for has been forked like this:
  weavejester/hiccup -> r0man/sablono -> Hicada."
  (:refer-clojure :exclude [compile])
  (:require
    cljs.analyzer
    [hicada.compiler.utils :as compiler]
    [hicada.env :as env]
    [hicada.infer :as infer]
    [hicada.convert :as runtime]
    [hicada.util :as util]
    [clojure.string :as str]))

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
  [[tag & body :as vec]]
  (let [[tag id class-string] (if (or (keyword? tag)
                                      (string? tag))
                                (runtime/parse-tag (name tag))
                                [tag nil nil])
        registered-element (get-in env/*options* [:custom-elements tag])
        is-element? (identical? (:create-element-tag env/*options*) tag)
        create-element? (or (some? registered-element)
                            is-element?
                            (string? tag)
                            (keyword? tag)
                            (= 'js (:tag (meta tag))))]
    (if create-element?
      (let [[tag body] (if is-element? [(first body) (rest body)]
                                       [tag body])
            props (first body)
            mode (props-mode props)
            props? (not= mode :no-props)]
        [(or registered-element tag)
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

(defmacro maybe-interpret-class [s]
  (if (= 'string (infer/infer-type s &env))
    s
    `(~(:convert-class env/*options*) ~s)))

(defmacro create-element [& args]
  `(.call ~(:create-element env/*options*) nil ~@args))

(declare literal->js)

(defn map-to-js [m]
  {:pre [(every? util/primitive? (keys m))]}
  (when (seq m)
    (let [kvs-str (->> (keys m)
                       (mapv #(str \' (literal->js %) "':~{}"))
                       (interpose ",")
                       (str/join))]
      (vary-meta
        (list* 'js* (str "{" kvs-str "}") (mapv literal->js (vals m)))
        assoc :tag 'object))))

(defn literal->js
  "Efficiently emit to literal JS form"
  [x]
  (cond
    (nil? x) x
    (keyword? x) (name x)
    (string? x) x
    (vector? x) (apply list 'cljs.core/array (mapv literal->js x))
    (map? x) (map-to-js x)
    :else x))

(declare emit)

(defn compile-vec
  "Returns an unevaluated form that returns a react element"
  [[tag :as form]]
  (let [[klass attrs children options] (analyze-vec form)]
    (emit klass attrs children options)))

(defn compile-or-interpret-child
  "Compiles hiccup forms & wraps ambiguous forms for runtime interpretation"
  [form]
  (if (map? form)
    (throw (ex-info "a map is an invalid child" {:form form}))
    (case (compile-mode form)
      :inline form
      :interpret `(~(:convert-form env/*options*) ~form)
      :compile (compile-vec form)
      :maybe-interpret
      (or (compiler/wrap-return form compile-or-interpret-child)
          `(infer/maybe-interpret ~form)))))

(defn compile-hiccup-child
  "Only compiles 'obvious' potential hiccup forms, ie. vectors... other args
   are untouched."
  [form]
  (or (compiler/wrap-return form compile-hiccup-child)
      (case (compile-mode form)
        :compile (compile-vec form)
        :interpret `(~(:convert-form env/*options*) ~form)
        form)))

(defn emit
  "Emits the final react js code"
  [tag props children {:keys [create-element?
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
                                                  `(~(:assoc-prop env/*options*) ~k ~v))
                                                (cond-> class-string
                                                        (conj `(~(:update-class env/*options*) ~class-string)))
                                                seq)]
                                 `(-> ~form ~@ops)
                                 form))]
    (if create-element?
      (list* `create-element
             tag
             (case prop-mode
               ;; literal, we can add static props at compile-time
               (:map :nil :no-props)
               (-> (into (or props {}) (filter val) {:id id :key key :ref ref})
                   runtime/convert-props
                   (cond-> class-string (update "className" #(cond (nil? %) class-string
                                                                   (string? %) (str class-string " " %)
                                                                   :else `(str ~(str class-string " ") ~%))))
                   literal->js)
               ;; dynamic clj, need to interpret & then add static props
               :dynamic
               (runtime-static-props
                 (when props
                   `(~(:convert-props env/*options*) ~props)))
               ;; skip interpret, but add static props
               :js-object
               (runtime-static-props props))
             (mapv compile-or-interpret-child children))
      ;; clj-element
      `(~'hicada.infer/maybe-interpret ~(with-meta `(~tag ~@(mapv compile-hiccup-child children)) form-meta)))))

(defn compile
  "Arguments:
  - content: The hiccup to compile
  - opts
   o :warn-on-interpretation? - Print warnings when code cannot be pre-compiled and must be interpreted at runtime? (Defaults to `true`)
   o :inlineable-types - CLJS type tags that are safe to inline without interpretation. Defaults to `#{'number 'string}`
   o :is-hiccup? (opt) fn of expr that returns true (interpret), false (skip), or nil (maybe-interpret)
   o :create-element 'hicada.convert/createElement - you can also use your own function here.
   o :camelcase-key-pred - defaults to (some-fn keyword? symbol?), ie. map keys that have
                           string keys, are NOT by default converted from kebab-case to camelCase!
  - tag-handlers:
   A map to handle special tags. Run before compile."
  ([content]
   (compile content nil))
  ([content options]
   (binding [env/*options* (env/with-defaults options)]
     (compile-or-interpret-child content))))

(defn make-macro [name options]
  (let [elements-sym (gensym (str name "-opts-elements"))
        as-element-sym (gensym (str name "-opts-as-element"))
        options (merge-with (fn [x y] (if (map? x) (merge x y) y)) options env/*options*)
        options (assoc options :tag/as-element-js as-element-sym
                               :tag/elements-js elements-sym)]
    `(do
       (def ~elements-sym (js-obj ~@(->> (:custom-elements options)
                                         (merge) (apply concat))))
       (defmacro ~name [body#]
         `(compile body#)))))

;; should be "b c a", order preserved


(defmacro as-element [body]
  (compile body))

(comment

  ;; High-Level Overview
  ;;
  ;; DOM tags are compiled to `react/createElement` calls.
  (compile '[:div])
  => (hicada.compiler/create-element "div" nil)

  ;; Symbol tags are compiled to function calls
  (compile '[my-fn 1 2 3])
  => (my-fn 1 2 3)

  ;; a literal map is compiled as props
  (compile '[:span {:on-click #()}])

  ;; a symbol or expression in 1st position is treated as a child element
  (compile '[:span a])
  (compile '[:span (a-fn)])

  ;; ...unless we tag it with :props metadata
  (compile-vec '[:span ^:props a])
  (compile-vec '[:span ^:props (a-fn)])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Props

  ;; keys are camelCase'd
  (runtime/convert-props {:on-click ()})

  ;; class vector is joined at compile time
  (runtime/convert-props {:class ["b" "c"]})
  ;; class vector may include dynamic elements
  (runtime/convert-props '{:class ["b" c]})
  ;; class may be dynamic - with runtime interpretation
  (runtime/convert-props '{:class x})
  ;; classes from tag + props are joined
  (compile [:h1.b.c {:class "a"}])
  ;; joining classes from tag + dynamic class forms
  (compile '[:div.c1 {:class x}])
  (compile '[:div.c1 {:class ["y" d]}])

  ;; style map is also converted to camel-case
  (runtime/convert-props '{:style {:font-weight 600}})
  ;; style map may be dynamic - with runtime interpretation
  (runtime/convert-props '{:style x})
  (compile '[:div {:style (assoc {} :width 10)}])
  ;; multiple style maps may be passed (for RN)
  (runtime/convert-props '{:style [{:font-size 10} x]})

  ;; some keys are handled as special cases when renamed
  (runtime/convert-props {:for "htmlFor"                    ;; special case
                          :class "className"                ;; special case
                          :kw-key "kwKey"                   ;; camelCase
                          :aria-key "aria-key"              ;; not camelCase (aria-*)
                          :data-key "data-key"              ;; not camelCase (data-*)
                          "string-key" "string-key"         ;; not camelCase (string)
                          })

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure vs React elements

  ;; a keyword tag is assumed to map to a DOM element and is compiled to createElement
  (compile-vec [:div])
  => (hicada.compiler/create-element "div" nil)

  ;; a symbol tag is assumed to be a regular function that returns a React element.
  ;; this is compiled to a regular function call - with no special handling of "props"
  (compile-vec '[my-fn 1 2 3])
  => (my-fn 1 2 3)

  ;; actually, the result is wrapped in `hicada.infer/maybe-interpret`.
  => (hicada.infer/maybe-interpret (my-fn 1 2 3))
  ;; more on that later.

  ;; arguments that look like hiccup forms are compiled unless tagged with ^:inline
  (compile-vec '[my-fn [:div]])
  (compile-vec '[my-fn [other-fn]])
  (compile-vec '[my-fn ^:inline [other-fn]])
  (compile-vec '[my-fn ^js [other-fn]])

  ;; to invoke a symbol with createElement (instead of calling it as a function),
  ;; use :>
  (compile-vec '[:> my-fn])
  ;; ... or use a ^js hint on the tag
  (compile-vec '[^js my-fn])

  => (hicada.compiler/create-element my-fn nil)

  ;; behind the scenes, `infer/inline?` determines whether a form is already
  ;; a valid React element (in which case, we skip runtime interpretation).

  ;; primitive strings, numbers, and nil are inlined:
  (infer/skip? "a string")                                ;; true
  (infer/skip? 1)                                         ;; true
  (infer/skip? nil)                                       ;; true
  ;; skip interpretation by adding ^:inline or ^js
  (infer/skip? '^:inline (my-fn))                         ;; true
  (infer/skip? '^js (my-fn))                              ;; true

  ;; using ^js allows us to rely on type propagation, eg. and define functions
  ;; whose return values will be inlined. the following is just an example, and
  ;; will not work here in a Clojure repl.
  (defn ^js my-fn [])
  ;; the ^js type can now be inferred at call sites,
  ;; and will not be wrapped with interpretation functions
  .. (my-fn)


  ;; Here are some more complete examples that include props and children.
  (compile-vec '[my-fn {:foo "bar"} a [:div]])
  (compile-vec '[:> my-fn {:foo "bar"} a [:div]])
  (compile-vec '[^js my-fn {:foo "bar"} a [:div]])

  ;; various ways to control interpretation/inlining of children
  (compile-vec '[:div
                 a                                          ;; maybe-interpret (not as props)
                 ^:interpret b                              ;; interpret-form
                 ^:inline c                                 ;; inline
                 ^js d])                                    ;; inline

  ;; in 'function mode' (as opposed to 'element mode'), only arguments that are vectors
  ;; are compile/interpreted:

  (compile-vec '[f a b [:div] c])

  ;; :<> compiles to a react Fragment, all children are compiled/interpreted
  (compile-vec '[:<> a b [:div] [x]])
  ;; ...with a react key
  (compile '^{:key "a"} [:<> a b])

  ;; interpret
  (compile-or-interpret-child 'b)                           ;; maybe-interpret symbol
  (compile-or-interpret-child '(hello [:div]))              ;; maybe-interpret unknown operator
  (compile-or-interpret-child '(do [:div]))                 ;; compile inner form
  (compile-or-interpret-child '(let [a 1]
                                 [:div a]                   ;; not compiled
                                 [:div b]))                 ;; compiled, `b` wrapped for runtime interpretation
  (compile-or-interpret-child '(for [x xs] [:span x]))      ;; compile inner return values
  (compile-or-interpret-child '(let [x (for [x [1 2 3]] x)] ;; only wrap-return at top level
                                 x))

  ;; key as metadata - only on element forms
  (compile-vec ^{:key 1} [:span "x"])
  (compile-vec ^{:key 1} [:span {:foo "bar"} "x"])          ;; with props map
  (compile-vec ^{:key 1} [:span 'd "x"])                    ;; with symbol child (unrelated)
  (compile-vec (quote ^{:key 1} [:span ^:props d "x"]))     ;; with dynamic props
  (compile '(for [x [1 2 3]]
              ^{:key (:y x)} [:span x]))                    ;; dynamic key

  ;; warning - key metadata is ignored because a-symbol is a function
  (compile-vec '^{:key 1} [a-symbol])

  ;; dynamic props with key and ref added at runtime
  (compile '^{:key "k" :ref "r"}
           [:div#id.class ^:props b c])

  (compile '[:div {:data-style (assoc {} :width 10)}])      ;; random props are not coerced to anything

  (compile 'hello)

  (map compile (list
                 [:Fragment]
                 [:<>]
                 [:Suspense]))

  )
