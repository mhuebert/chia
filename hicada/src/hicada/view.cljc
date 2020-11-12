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
                            [hicada.compiler :as c])))

(reset! env/default-options
        (env/parse-opts
          {;; settings for the compiler:
           :warn-on-interpretation? true
           :skip-types '#{number
                          string
                          function
                          js}
           :rewrite-for? true

           ;; relevant for the interpreter:
           :create-element-tag ">"
           :custom-elements {"Fragment" hicada.react/Fragment
                             "<>" hicada.react/Fragment
                             "Suspense" hicada.react/Suspense}
           :create-element hicada.react/createElement
           :convert-form hicada.convert/as-element
           :convert-props hicada.convert/convert-props
           :convert-class hicada.convert/class-string
           :update-class hicada.convert/update-class!
           :assoc-prop applied-science.js-interop/assoc!}))

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
                                    (~'hicada.compiler/as-element ~(last body)))
                                  (j/!set :displayName ~qualified-name))
             ~@(when key-fn [key-fn-sym key-fn])]
         (defn ~name
           ~@(when docstring [docstring])
           {:arglists (~argv)}
           ~simple-args
           (~'hicada.compiler/create-element
             ~constructor-sym
             ~(when key-fn `(j/obj :key (~key-fn-sym ~(first argv))))
             ~@simple-args))
         (when ~'hicada.view/refresh-enabled?
           ;; type, key, forceReset, getCustomHooks
           (~signature-sym ~name ~(hook-signature body) nil nil)
           (~'hicada.view/register! ~name (j/!get ~name :displayName)))
         #'~name)))

  (defmacro as-element [form]
    `(~'hicada.compiler/as-element ~form)))
