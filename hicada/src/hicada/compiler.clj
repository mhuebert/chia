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
    [hicada.normalize :as norm]
    [hicada.util :as util]
    [clojure.string :as str]))

(defn- primitive?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (or (string? x)
      (keyword? x)
      (number? x)
      (nil? x)))

(defn is-hiccup? [form]
  (if (primitive? form)
    false
    (let [{:keys [hiccup not-hiccup tag]} (meta form)]
      (cond not-hiccup false
            (= tag 'js) false
            (false? hiccup) false
            (true? hiccup) true
            :else nil))))

(defmacro ensure-class-string [s]
  (if (= 'string (infer/infer-type s &env))
    s
    `(~'hicada.interpreter/classes-string ~s)))

(declare emit compile-or-interpret-form)

(defn compile-vec
  "Returns an unevaluated form that returns a react element"
  [[tag :as form]]
  (let [handler (get-in env/*options* [:tag-handlers tag])
        handled-form (cond-> form handler (handler))
        [klass attrs children options] (norm/hiccup-vec handled-form)]
    (emit klass attrs children options)))

(defn compile-other
  [expr]
  (let [{:keys [is-hiccup?
                interpret]
         :or {is-hiccup? is-hiccup?}} env/*options*]
    (or (compiler/wrap-return expr compile-or-interpret-form)
        (case (is-hiccup? expr)
          false expr
          true `(~interpret ~expr)
          nil `(infer/maybe-interpret ~expr)))))

(defn compile-or-interpret-form
  "Compiles hiccup forms & wraps ambiguous forms for runtime interpretation"
  [form]
  (cond
    (vector? form) (compile-vec form)
    (primitive? form) form
    (map? form) (throw (ex-info "a map is an invalid form" {:form form}))
    :else (compile-other form)))

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
  {:pre [(every? primitive? (keys m))]}
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
  [tag props children {:keys [js-element?
                              id
                              class-string
                              key
                              ref
                              prop-mode]}]
  (let [{:keys [create-element]} env/*options*
        static-props (util/assoc-some nil
                                      :id id
                                      :key key
                                      :ref ref)
        emit-static-props (fn [form]
                            (let [ops (keep identity
                                            [(when (seq static-props)
                                               `(~'applied-science.js-interop/assoc!
                                                  ~@(apply concat static-props)))
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

    (if js-element?
      ;; js-element: everything needs to be compiled
      (list* create-element
             tag
             (case prop-mode
               ;; literal props, we can add static props at compile-time
               (:map :nil)
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
               (emit-static-props props)
               :no-props nil)
             (mapv compile-or-interpret-form children))
      ;; clj-element
      (list* create-element
             tag
             (to-js static-props)                           ;; only static props are possible (:key, :ref as metadata)
             (mapv compile-form children))                  ;; only hiccup forms are compiled (no wrapping of interpret)
      )))

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
   (compile content nil))
  ([content options]
   (binding [env/*options* (env/with-defaults options)]
     (compile-or-interpret-form content))))

;; should be "b c a", order preserved

(defmacro to-element [body]
  (compile body))

(comment

  ;; Props
  (compile-props {:class ["b" "c"]})                        ;; join static class vector
  (compile-props {:class ["b" 'c]})                         ;; join dynamic class vector
  (compile-props '{:class x})                               ;; join dynamic class symbol

  (compile-props {:style {:border-width "2px"}})            ;; camelCase style
  (compile-props {:style [{:border-width "2px"}]})          ;; camelcase vector of style maps (RN)
  (compile-props {:on-click ()})                            ;; camelCase event

  (compile-props {:for "htmlFor"
                  :aria-x "aria-x"
                  :data-x "data-x"
                  :other-k "otherK"})                       ;; prop renaming


  ;; JS vs CLJ elements
  (compile-vec '[my-fn {:foo "bar"} a [:div]])              ;; symbol => pass children as unprocessed props
  (compile-vec '[:js my-fn {:foo "bar"} a [:div]])          ;; :js => process all args

  ;; Children
  (compile-vec '[:div
                 a                                          ;; maybe-interpret (not as props)
                 ^:hiccup b                                 ;; interpret
                 ^:not-hiccup c                             ;; pass
                 ^js d])                                    ;; pass

  ;; fragments
  (compile-vec '[:<> a b [:div]])                           ;; all children compiled/interpreted

  ;; interpret
  (compile-or-interpret-form 'b)                            ;; maybe-interpret symbol
  (compile-or-interpret-form '(hello [:div]))               ;; maybe-interpret unknown operator
  (compile-or-interpret-form '(do [:div]))                  ;; compile known operator
  (compile-or-interpret-form '(let [a 1] [:div a]))         ;; maybe-interpret bound symbol

  ;; use :props metadata to indicate dynamic props
  (compile-vec '[:span a b])                                ;; 2 children
  (compile-vec '[:span ^:props a b])                        ;; props + child

  (compile-or-interpret-form '(for [x xs] [:span x]))       ;; compile inner return values
  (compile-or-interpret-form '(let [x (for [x [1 2 3]] x)]  ;; only wrap-return at top level
                                x))

  ;; key & ref as metadata
  (compile-vec '^{:key "k" :ref "r"} [my-fn {:prop 1}])     ;; handle key and ref props
  (compile-vec ^{:key 1} [:span "x"])
  (compile-vec ^{:key 1} [:span {:foo "bar"} "x"])          ;; with props map
  (compile-vec ^{:key 1} [:span 'd "x"])                    ;; with symbol child (unrelated)
  (compile-vec (quote ^{:key 1} [:span ^:props d "x"]))     ;; with dynamic props
  (compile '(for [x [1 2 3]]
              ^{:key (:y x)} [:span x]))                    ;; dynamic key


  (require 'hicada.interpreter)

  (compile [:h1.b.c {:class "a"}])                          ;; classes from tag + props are joined
  (compile [:h1.b.c {:class-name "a"}])                     ;; ** class-name is ignored

  ;; fragments
  (compile '[:<> a b])                                      ;; all children are interpreted
  (compile '^{:key "a"} [:<> a b])                          ;; with key

  ;; :js
  (compile '[:js SomeView props b])                         ;; in :js mode, all children are compiled/interpreted
  (compile '[:js SomeView ^:props props b])                     ;; ^js for props
  (compile '^{:key 1} [:js SomeView ^js props b])

  ;; Doesn't convert string keys, but do convert keywords & symbols:
  (compile '[:div {"kebab-case" y :camel-case x camel-case-2 8}])

  (compile '[:js Transition {:in in-prop} (fn [state])])    ;; works eq to :>
  (compile '[clj-view ^:props b c])
  (compile '^{:key "k" :ref "r"} [:div#id.class ^:props b c])
  (compile '[:div.c1 {:class x} "D"])
  (compile '[:div.c1 {:class ["y" d]}])
  (compile '(some-fn {:in in-prop} (fn [state])))           ;; FN call, don't touch

  (compile '[:Text {:style [{:border-bottom "2px"}]}])

  (compile '[:div {:style (assoc {} :width 10)}])           ;; style object is camelCased
  (compile '[:div {:data-style (assoc {} :width 10)}])      ;; random props are not traversed

  (compile '[:div (assoc {} :style {:width 10})])           ;; not a literal map, interpreted as a chile
  (compile '[:div ^:props (assoc {} :style {:width 10})])   ;; props tag

  )
