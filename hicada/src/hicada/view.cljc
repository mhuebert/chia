(ns hicada.view
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [cljs.analyzer :as ana]
            [applied-science.js-interop :as j]
            #?@(:clj [[hicada.compiler :as c]
                      [net.cgrand.macrovich :as m]])
            #?(:cljs [hicada.interpreter]))
  #?(:cljs (:require-macros [net.cgrand.macrovich :as m]
                            [hicada.compiler :as c])))

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
    ;; identifies lexical occurrences of symbols that begin with `use`
    ;; followed by - or any capital letter. Whenever this "hook signature"
    ;; changes
    (->> (find-all (every-pred symbol? maybe-hook?) body)
         (str/join "|")))

  (defn simple-argv [argv]
    (mapv #(cond (symbol? %) %
                 (map? %) (gensym "map")
                 (vector? %) (gensym "vec")
                 :else (gensym)) argv))

  (defmacro defview [name & args]
    (let [[docstring opts argv & body] (parse-args args string? map?)
          qualified-name (str *ns* "/" name)
          signature-sym (gensym (str name "-signature"))
          constructor-sym (symbol (str name "-fn"))
          key-fn (:key opts)
          key-fn-sym (gensym (str name "-keyfn"))
          single? (= 1 (count argv))
          name (vary-meta name assoc :tag 'js)
          simple-args (simple-argv argv)]
      `(do
         (when ~'hicada.view/refresh-enabled?
           (def ~signature-sym (~'hicada.view/signature-fn)))

         ~@(when key-fn `[(def ~key-fn-sym ~key-fn)])

         (j/defn ~constructor-sym                           ;; name
           [^:js {~(if single?
                     (vary-meta (first argv) assoc :clj true)
                     (with-meta argv {:js/shallow true})) :children :as p#}]
           (when ~'triple.view/refresh-enabled? (~signature-sym))
           ~@(drop-last body)
           (~'hicada.compiler/to-element ~(last body)))

         (-> ~constructor-sym
             (j/!set :displayName ~qualified-name))

         (defn ~name
           ~@(when docstring [docstring])
           {:arglists (~argv)}
           ~simple-args
           ~(list* 'hicada.interpret/createElement
                   constructor-sym
                   (when key-fn `(j/obj :key (~key-fn-sym ~(first argv))))
                   simple-args))
         (when ~'triple.view/refresh-enabled?
           ;; type, key, forceReset, getCustomHooks
           (~signature-sym ~name ~(hook-signature body) nil nil)
           (~'triple.view/register! ~name (j/!get ~name :displayName)))
         )))

  (defmacro to-element [form]
    `(~'hicada.compiler/to-element ~form))

  #_(defmacro defview [name & args]
      (let [[doc options argv & body] (parse-args args string? map?)
            keyfn (:key options)]
        `(defn ~(vary-meta name assoc :tag 'js) ~@(keep identity [doc options]) ~argv
           ~@(butlast body)
           (c/to-element ~(with-meta (last body) {:key `(~keyfn (first argv))}))))) )
