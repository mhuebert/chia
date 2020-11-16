(ns hicada.view
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [cljs.analyzer :as ana]
            [applied-science.js-interop :as j]
            [hicada.env :as env]
            #?@(:clj [[hicada.compiler :as c]
                      [net.cgrand.macrovich :as m]])
            #?(:cljs [hicada.convert]))
  #?(:cljs (:require-macros [net.cgrand.macrovich :as m]
                            [hicada.compiler :as c]
                            hicada.view)))

#?(:cljs
   (do
     (def ^boolean refresh-enabled?
       (and goog/DEBUG (exists? js/ReactRefresh)))

     (defn register!
       "Registers a component with the React Fresh runtime.
       `type` is the component function, and `id` is the unique ID assigned to it
       (e.g. component name) for cache invalidation."
       [type id]
       (when refresh-enabled?
         (j/call js/ReactRefresh :register type id)))

     (defn signature-fn []
       (when refresh-enabled?
         (j/call js/ReactRefresh :createSignatureFunctionForTransform)))))


(env/def-options hiccup-opts {})

(m/deftime

  (defn parse-args [args & preds]
    (loop [args args
           preds preds
           out []]
      (if (and (seq args) (seq preds))
        (let [p (first preds)]
          (if (p (first args))
            (recur (rest args)
                   (rest preds)
                   (conj out (first args)))
            (recur args
                   (rest preds)
                   (conj out nil))))
        (into out args))))

  (defn- find-all [pred body]
    (let [sym-list (atom [])]
      (walk/postwalk
        (fn w [x]
          (if (pred x)
            (do (swap! sym-list conj x)
                x)
            x))
        body)
      @sym-list))

  (defn- maybe-hook?
    ;; it's ok to be liberal in what we accept - the signature only changes when
    ;; editing source code.
    [sym]
    (let [sym-name (name sym)]
      (or
        (identical? "deref" sym-name)
        (str/starts-with? sym-name "use")
        (some->> (ana/resolve-symbol sym)
                 (namespace)
                 (str/includes? "hook")))))

  (defn- hook-signature [body]
    ;; a string containing all symbols in body that begin with `use`
    ;; followed by - or any capital letter, joins to a string.
    (->> (find-all (every-pred symbol? maybe-hook?) body)
         (str/join "|")))

  (defn simple-argv [argv]
    (mapv #(cond (symbol? %) %
                 (map? %) (:as % (gensym "map"))
                 (vector? %) (gensym "vec")
                 :else (gensym)) argv))

  (defmacro as-element [form]
    (c/compile hiccup-opts form))

  (defmacro defview [name & args]
    (let [[docstring opts argv & body] (parse-args args string? map?)
          qualified-name (str *ns* "/" name)
          signature-sym (gensym (str name "-signature"))
          constructor-sym (symbol (str name "-fn"))
          key-fn (:key opts)
          key-fn-sym (gensym (str name "-keyfn"))
          name (vary-meta name assoc :tag 'js)
          simple-args (simple-argv argv)]
      `(let [~signature-sym (when ~'hicada.view/refresh-enabled? (~'hicada.view/signature-fn))
             ~constructor-sym (-> (j/fn ~constructor-sym    ;; name
                                    [^:js {~(if (= 1 (count argv))
                                              (vary-meta (first argv) assoc :clj true)
                                              (with-meta argv {:js/shallow true})) :children :as p#}]
                                    (when ~'hicada.view/refresh-enabled? (~signature-sym))
                                    ~@(drop-last body)
                                    (c/as-element hiccup-opts ~(last body)))
                                  (j/!set :displayName ~qualified-name))
             ~@(when key-fn [key-fn-sym key-fn])]
         (defn ~name
           ~@(when docstring [docstring])
           {:arglists (~argv)}
           ~simple-args
           (~@(:create-element-compile hiccup-opts)
             ~constructor-sym
             ~(when key-fn `(j/obj :key (~key-fn-sym ~(first argv))))
             ~@simple-args))
         (when ~'hicada.view/refresh-enabled?
           ;; type, key, forceReset, getCustomHooks
           (~signature-sym ~name ~(hook-signature body) nil nil)
           (~'hicada.view/register! ~name (j/!get ~name :displayName)))
         #'~name))))

