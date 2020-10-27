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

(defn compile-props
  "Compile a HTML attribute map to react (class -> className), camelCases :style."
  [attrs]
  (if (map? attrs)
    (reduce-kv (fn [m k v] (util/compile-prop assoc m k v)) {} attrs)
    attrs))

(comment
  (compile-props {:class ["b" 'c] :style {:border-width "2px"}}) ;; camelcase style
  ;; React native style:
  (compile-props {:class ["b" 'c] :style [{:border-width "2px"}]}) ;; camelcase style
  (compile-props {:on-click ()}))

(declare compile-html)

(defn- primitive?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (or (string? x)
      (keyword? x)
      (number? x)
      (nil? x)))

(defn is-inline? [form]
  (or
    (primitive? form)
    (let [form-meta (meta form)]
      (cond (:inline form-meta) true
            (:interpret form-meta) false
            :else nil))))

(declare emit compile-form)

(defn compile-vec
  "Returns an unevaluated form that returns a react element"
  [[tag :as form]]
  (let [handler (get-in env/*options* [:handlers tag] norm/hiccup-vec)
        [klass attrs children options] (handler form)]
    (emit klass attrs children options)))

(comment
  (compile-vec '[:div (for [x xs] [:span x])])

  (compile-vec '[:> A {:foo "bar"} a])
  (compile-vec '[:<> A a b])
  (compile-form 'b)
  (compile-vec '[A {:foo "bar"}
                 [:span a]])
  (compile-vec '[A b a])
  (compile-vec '[:* 0 1 2])
  (compile-vec '(array [:div "foo"] [:span "foo"])))

(defn compile-other
  [expr]
  (let [{:keys [is-inline?
                interpret]
         :or {is-inline? is-inline?}} env/*options*]
    (prn :compile-other expr (compiler/wrap-return expr compile-form))
    (or (compiler/wrap-return expr compile-form)
        (case (is-inline? expr)
          true expr
          false `(~interpret ~expr)
          nil `(infer/maybe-interpret ~expr)))))

(defn compile-form
  "Pre-compile data structures"
  [form]
  (cond
    (vector? form) (compile-vec form)
    (primitive? form) form
    (map? form) (throw (ex-info "a map is an invalid form" {:form form}))
    :else (compile-other form)))

(comment
  (compile-form `(hello [:div props "X"]))                  ;; wraps form with `interpret-when-necessary`
  (compile-form '[:div {:class "b"} x])                     ;; child is
  (compile-form '[my-fn {:class "b"} x]))

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
    (keyword? x) (name x)
    (string? x) x
    (vector? x) (apply list 'cljs.core/array (mapv to-js x))
    (map? x) (map-to-js x)
    :else x))

(defn emit
  "Emits the final react js code"
  [tag props children {:keys [compile-children?]}]
  (let [{:keys [create-element]} env/*options*
        props (cond (map? props) (to-js (compile-props props))
                    (and (seq? props)
                         (= 'hicada.interpreter/props (first props))) props
                    :else `(~'hicada.interpreter/props ~props))]
    (list* create-element tag props (cond->> children
                                             compile-children?
                                             (mapv compile-form)))))

(defn compile
  "Arguments:
  - content: The hiccup to compile
  - opts
   o :warn-on-interpretation? - Print warnings when code cannot be pre-compiled and must be interpreted at runtime? (Defaults to `true`)
   o :inlineable-types - CLJS type tags that are safe to inline without interpretation. Defaults to `#{'number 'string}`
   o :is-inline? (opt) fn of expr that returns true (inline), false (interpret), or nil (maybe-interpret)
   o :create-element 'js/React.createElement - you can also use your own function here.
   o :camelcase-key-pred - defaults to (some-fn keyword? symbol?), ie. map keys that have
                           string keys, are NOT by default converted from kebab-case to camelCase!
  - handlers:
   A map to handle special tags. See default-handlers in this namespace.
  - env: The macro environment. Not used currently."
  ([content]
   (compile content nil))
  ([content options]
   (binding [env/*options* (env/with-defaults options)]
     (compile-form content))))

;; should be "b c a", order preserved

(defmacro to-element [body]
  (compile body))

(comment

  (defmacro t [x]
    (tap> [&env])
    (str (infer/infer-type x &env)))
 (tap> 8)
  ;; works
  (t ""
     )
  (t ())
  (t [])
  (t {})
  (t (str ""))
  )

(comment
  (require 'hicada.interpreter)

  (compile [:h1.b.c {:class "a"}])

  ;; these no longer work - only :class is supported
  (compile [:h1.b.c {:class-name "a"}])

  (macroexpand '(to-element [:div (for [x xs]
                                   [:span x])]))
  (compile '[:div (for [x xs]
                    [:span x])])

  (compile '[:<> {:key "a"} a b])
  (compile '[:<> a b])
  (compile '[:> :div props b])
  (compile '[:> :div {:a "b"} b])

  ;; Doesn't convert string keys, but do convert keywords & symbols:
  (compile '[X {"kebab-case" y :camel-case x camel-case-2 8}])

  (compile '[Transition {:in in-prop} (fn [state])])        ;; works eq to :>
  (compile '[a b c])                                        ;; We have a coll of ReactNodes. Don't touch
  (compile '[a ^:props b c])
  (compile '[:div#id.class ^:props b c])
  (compile '[:div.c1 {:class x} "D"])
  (compile '[:div.c1 {:class ["y" d]}])
  (compile '(some-fn {:in in-prop} (fn [state])))           ;; FN call, don't touch

  (compile
    '[:> Transition {:in in-prop
                     :unmount-on-exit true
                     :timeout {:enter 300, :exit 100}}
      (fn [state])])

  ;; Issue #2:
  ;; DEPRECATED - :before-emit is no lonter
  (compile '[:div {:ihtml "<div>hi</div>"}]
           {:before-emit (fn [[tag attr ch]]
                           (if-some [html (:ihtml attr)]
                             [tag
                              (-> attr
                                  (dissoc :ihtml)
                                  (assoc :dangerouslySetInnerHTML {:__html html}))
                              ch]
                             [tag attr ch]))})

  (compile '[:Text {:style [{:border-bottom "2px"}]}])

  (compile '[:div {:style (assoc {} :width 10)}])
  (compile '[:div {:data-style (assoc {} :width 10)}])
  (compile '[:div (assoc {} :style {:width 10})])

  )

;; CHANGES
;; - pass :wrap-interpret, was `(hicada.interpreter/interpret ~expr)
