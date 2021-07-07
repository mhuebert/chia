(ns triple.view
  (:require [applied-science.js-interop :as j]
            [cljs.analyzer :as ana]
            [clojure.walk :as walk]
            [clojure.string :as str]))

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

(comment
  ;; future use
  (defn- find-locals [{:as env :keys [locals]} body]
    (into []
          (comp (distinct)
                (keep #(:name (locals %))))
          (find-all symbol? body))))

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

(defmacro defview [display-name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (rest args)]
                           [nil args])
        [opts args] (if (map? (first args))
                      [(first args) (rest args)]
                      [nil args])
        [argv & body] args
        qualified-name (str *ns* "/" display-name)
        signature      (gensym (str display-name "-signature"))]
    `(do

       (when ~'triple.view/refresh-enabled?
         (def ~signature (~'triple.view/signature-fn)))

       (def ~display-name                                   ;; name
         ~@(when docstring [docstring])                     ;; docstring

         (-> (j/fn [^:js {children# :children}]             ;; arglist
               (j/let [children# (if (~'cljs.core/array? children#)
                                   children#
                                   (~'cljs.core/array children#))
                       ~(with-meta argv {:js/shallow true}) children#]
                 (when ~'triple.view/refresh-enabled? (~signature))
                 ~@(drop-last body)
                 (~'triple.view.hiccup/to-element ~(last body))))
             (j/!set :displayName ~qualified-name)
             (~'triple.view.hiccup/mark-clj-view!)))

       (when ~'triple.view/refresh-enabled?
         ;; type, key, forceReset, getCustomHooks
         (~signature ~display-name ~(hook-signature body) nil nil)
         (~'triple.view/register! ~display-name ~qualified-name))

       ~display-name)))

(defmacro to-element [form]
  `(~'triple.view.hiccup/to-element ~form))