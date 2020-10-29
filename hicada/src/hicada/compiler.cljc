(ns hicada.compiler
  "
  Hicada - Hiccup compiler aus dem Allgaeu

  NOTE: The code for has been forked like this:
  weavejester/hiccup -> r0man/sablono -> Hicada."
  (:refer-clojure :exclude [compile])
  (:require
    cljs.analyzer
    [hicada.compiler.impl :as compiler]
    [hicada.compiler.env :as env]
    [hicada.infer :as infer]
    [hicada.interpreter :as interpret]
    [hicada.normalize :as norm]
    [hicada.util :as util]
    [clojure.string :as str]
    #?(:clj [net.cgrand.macrovich :as m]))
  #?(:cljs (:require-macros hicada.compiler
                            [net.cgrand.macrovich :as m])))

(defn should-inline? [form]
  (if (util/primitive? form)
    true
    (let [{:keys [inline interpret tag]} (meta form)]
      (cond inline true
            interpret false
            (= tag 'js) true
            :else nil))))

(defn should-interpret? [form]
  (if (util/primitive? form)
    false
    (let [{:keys [inline interpret tag]} (meta form)]
      (cond inline false
            interpret true
            (= tag 'js) false
            :else nil))))

(defmacro ensure-class-string [s]
  (if (= 'string (infer/infer-type s &env))
    s
    `(~'hicada.interpreter/classes-string ~s)))

(declare emit)

(defn compile-vec
  "Returns an unevaluated form that returns a react element"
  [[tag :as form]]
  (let [handler (get-in env/*options* [:tag-handlers tag])
        handled-form (cond-> form handler (handler))
        [klass attrs children options] (norm/hiccup-vec handled-form)]
    (emit klass attrs children options)))

(defn compile-or-interpret-child
  "Compiles hiccup forms & wraps ambiguous forms for runtime interpretation"
  [form]

  (cond (map? form) (throw (ex-info "a map is an invalid child" {:form form}))
        (should-inline? form) form
        (should-interpret? form) `(~'hicada.interpreter/interpret ~form)
        (vector? form) (compile-vec form)
        :else
        (or (or (compiler/wrap-return form compile-or-interpret-child)
                `(infer/maybe-interpret ~form)))))

(defn compile-form
  "Compiles hiccup forms"
  [form]
  (cond-> form
          (util/hiccup-vec? form)
          (compile-vec)))

(defn compile-map-children [form]
  (reduce-kv (fn [m k v]
               (if (util/hiccup-vec? v)
                 (assoc m k (compile-vec v))
                 m)) form form))

(declare to-js)

(defn map-to-js [m]
  {:pre [(every? util/primitive? (keys m))]}
  (when-not (empty? m)
    (let [kvs-str (->> (keys m)
                       (mapv #(str \' (to-js %) "':~{}"))
                       (interpose ",")
                       (str/join))]
      (vary-meta
        (list* 'js* (str "{" kvs-str "}") (mapv to-js (vals m)))
        assoc :tag 'object))))

(defn to-js
  "Efficiently emit to literal JS form"
  [x]
  (cond
    (nil? x) x
    (keyword? x) (name x)
    (string? x) x
    (vector? x) (apply list 'cljs.core/array (mapv to-js x))
    (map? x) (map-to-js x)
    :else x))

(defn compile-props
  "Compile a HTML attribute map to react (class -> className), camelCases :style."
  [attrs]
  (reduce-kv (fn [m k v]
               (util/compile-prop assoc m k v)) {} attrs))

(defn emit
  "Emits the final react js code"
  [tag props children {:keys [element?
                              id
                              class-string
                              key
                              ref
                              prop-mode
                              form-meta]}]
  (let [{:keys [create-element]} env/*options*
        static-props (seq (apply concat (util/assoc-some nil
                                                         :id id
                                                         :key key
                                                         :ref ref)))
        emit-static-props (fn [form]
                            (let [ops (keep identity
                                            [(when (seq static-props)
                                               `(~'applied-science.js-interop/assoc!
                                                  ~@static-props))
                                             (when class-string
                                               `(~'hicada.interpreter/update-class! ~class-string))])]
                              (if (seq ops)
                                `(-> ~form ~@ops)
                                form)))]

    ;; meta-key - comes from meta, need to add (if :map or :nil, at compile, otherwise, at runtime)
    ;; dynamic-key - extract from props at runtime? (can we ignore this?, only use meta, or handled by component?)
    ;; ref - in case of clj, need to extract from props (if a map, statically, if dynamic, at runtime)
    ;;       and add it to props object
    ;;
    ;; js-only --
    ;;   class-name: comes from tag, need to add (if :map or :nil, at compile, else at runtime)
    ;;   id: comes from tag, need to add (if :map or :nil, at compile, else at runtime)
    (if element?
      ;; js-element: everything needs to be compiled
      (do
        (tap> [:prop-mode prop-mode :cs class-string])
        (list* create-element
               tag
               (case prop-mode
                 ;; literal props, we can add static props at compile-time
                 (:map :nil :no-props)
                 (-> props
                     (util/assoc-some :id id :key key :ref ref)
                     (cond-> class-string (update :class norm/conj-class class-string))
                     compile-props
                     to-js)
                 ;; dynamic clj, need to interpret
                 :dynamic-map
                 (emit-static-props
                   (when props
                     (if (util/interpreted? props)
                       props
                       `(~'hicada.interpreter/props ~props))))
                 ;; don't compile js objects, but do add static data if present
                 :js-object
                 (emit-static-props props))
               (mapv compile-or-interpret-child children)))
      ;; clj-element
      `(~'hicada.infer/maybe-interpret ~(with-meta `(~tag ~@(mapv compile-form children)) form-meta)))))

(defn compile
  "Arguments:
  - content: The hiccup to compile
  - opts
   o :warn-on-interpretation? - Print warnings when code cannot be pre-compiled and must be interpreted at runtime? (Defaults to `true`)
   o :inlineable-types - CLJS type tags that are safe to inline without interpretation. Defaults to `#{'number 'string}`
   o :is-hiccup? (opt) fn of expr that returns true (interpret), false (skip), or nil (maybe-interpret)
   o :create-element 'js/React.createElement - you can also use your own function here.
   o :camelcase-key-pred - defaults to (some-fn keyword? symbol?), ie. map keys that have
                           string keys, are NOT by default converted from kebab-case to camelCase!
  - tag-handlers:
   A map to handle special tags. Run before compile."
  ([content]
   (tap> [:compiling content])
   (compile content nil))
  ([content options]
   (binding [env/*options* (env/with-defaults options)]
     (compile-or-interpret-child content))))

;; should be "b c a", order preserved

(defmacro to-element [body]
  (compile body))

(comment

  ;; High-Level Overview
  ;;
  ;; DOM tags are compiled to `react/createElement` calls.
  (compile '[:div])
  => (hicada.interpreter/createElement "div" nil)

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
  (compile-props {:on-click ()})

  ;; class vector is joined at compile time
  (compile-props {:class ["b" "c"]})
  ;; class vector may include dynamic elements
  (compile-props '{:class ["b" c]})
  ;; class may be dynamic - with runtime interpretation
  (compile-props '{:class x})
  ;; classes from tag + props are joined
  (compile [:h1.b.c {:class "a"}])
  ;; warning - :class-name is ignored
  (compile [:h1.b.c {:class-name "a"}])
  ;; joining classes from tag + dynamic class forms
  (compile '[:div.c1 {:class x} "D"])
  (compile '[:div.c1 {:class ["y" d]}])

  ;; style map is also converted to camel-case
  (compile-props '{:style {:font-weight 600}})
  ;; style map may be dynamic - with runtime interpretation
  (compile-props '{:style x})
  (compile '[:div {:style (assoc {} :width 10)}])
  ;; multiple style maps may be passed (for RN)
  (compile-props '{:style [{:font-size 10} x]})

  ;; some keys are handled as special cases when renamed
  (compile-props {:for "htmlFor"                            ;; react-specific
                  :class "className"
                  :aria-key "aria-key"                      ;; not camelCased
                  :data-key "data-key"                      ;; not camelCased
                  "string-key" "string-key"})               ;; not camelCased

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure vs React elements

  ;; a keyword tag is assumed to map to a DOM element and is compiled to createElement
  (compile-vec [:div])
  => (hicada.interpreter/createElement "div" nil)

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

  ;; to invoke a symbol with createElement (instead of calling it as a function),
  ;; add a ^js hint or use `:>` as the tag
  (compile-vec '[^js my-fn])
  (compile-vec '[:> my-fn])
  => (hicada.interpreter/createElement my-fn nil)

  ;; behind the scenes, `infer/inline?` determines whether a form is already
  ;; a valid React element (in which case, we skip runtime interpretation).

  ;; primitive strings, numbers, and nil are inlined:
  (infer/inline? "a string")                                ;; true
  (infer/inline? 1)                                         ;; true
  (infer/inline? nil)                                       ;; true
  ;; skip interpretation by adding ^:inline or ^js
  (infer/inline? '^:inline (my-fn))                         ;; true
  (infer/inline? '^js (my-fn))                              ;; true

  ;; using ^js allows us to rely on type propagation, eg. and define functions
  ;; whose return values will be inlined. the following is just an example, and
  ;; will not work here in a Clojure repl.
  (defn ^js my-fn [])
  ;; the ^js type can now be inferred at call sites
  .. (my-fn)                                                ;; inferred tag will be 'js


  ;; Here are some more complete examples that include props and children.
  (compile-vec '[my-fn {:foo "bar"} a [:div]])
  (compile-vec '[:> my-fn {:foo "bar"} a [:div]])
  (compile-vec '[^js my-fn {:foo "bar"} a [:div]])

  ;; various ways to control interpretation/inlining of children
  (compile-vec '[:div
                 a                                          ;; maybe-interpret (not as props)
                 ^:interpret b                              ;; interpret
                 ^:inline c                                 ;; inline
                 ^js d])                                    ;; inline

  ;; :<> compiles to a react Fragment, children are compiled/interpreted
  (compile-vec '[:<> a b [:div] [x]])
  ;; ...with a react key
  (compile '^{:key "a"} [:<> a b])

  ;; interpret
  (compile-or-interpret-child 'b)                           ;; maybe-interpret symbol
  (compile-or-interpret-child '(hello [:div]))              ;; maybe-interpret unknown operator
  (compile-or-interpret-child '(do [:div]))                 ;; compile inner form
  (compile-or-interpret-child '(let [a 1] [:div a]))        ;; compile inner form, maybe-interpret unknown symbol
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

  )
