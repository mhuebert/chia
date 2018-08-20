(ns chia.util
  (:require [chia.util.js-interop :as j]))

(defmacro for-map [& body]
  `(->> (for ~@body)
        (apply merge)))

(defmacro doto->>
  "Like `doto`, but threads the value of `x` through the end of each expression."
  {:added "1.0"}
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (with-meta
                 (if (seq? f)
                   `(~(first f) ~@(next f) ~gx)
                   `(~f ~gx))
                 (meta f)))
              forms)
       ~gx)))

(defn munged-key [k]
  (munge (str (when (keyword? k)
                (some-> (namespace k)
                        (str "__")))
              (name k))))

(defmacro memoized-on [o k & body]
  (let [k (munged-key k)]
    `(or (j/get ~o ~k)
         (doto->> (do ~@body)
                  (j/assoc! ~o ~k)))))

(defn user-bindings
  "Returns all user-assigned bindings resulting from a let binding."
  [let-bindings]
  (let [;; set of symbols that will be bound via clojure.core/destructure
        bound-sym? (->> (clojure.core/destructure let-bindings) ;; not selfhost-compatible
                        (partition 2)
                        (map first)
                        (set))
        ;; all keywords & symbols that appear in the user destructuring side
        user-syms-keywords (->> (partition 2 let-bindings)
                                (map first)
                                (tree-seq coll? seq)
                                (filter #(or (symbol? %)
                                             (keyword? %)))
                                (distinct))]
    (->> user-syms-keywords
         ;; only keep symbols/keywords which correspond to bound names
         ;; (ie. ignore generated symbols)
         (filter (fn [x]
                   (-> (name x)
                       (symbol)
                       (bound-sym?)))))))

(comment
 (= (user-bindings '[a 4
                     {:as m
                      :keys [b :c ::d x/e]
                      [f] :n
                      g :o} {}])
    '[a m b :c ::d x/e f g]))

(defmacro log-let [bindings & body]
  (let [{:keys [file line]} (meta bindings)]
    `(let ~bindings
       (~'js/console.groupCollapsed ~(str file "#" line))
       ~@(for [user-binding (user-bindings bindings)]
           `(~'js/console.log (quote ~user-binding) ~(symbol (name user-binding))))
       (~'js/console.groupEnd)
       ~@body)))

(defmacro log-sym [sym]
  `(do (~'js/console.log ~(str sym ":") ~sym)))

(defmacro as-promise [expr]
  `(~'js/Promise.
    (fn [resolve# reject#]
      (~@expr (fn [err# result#]
                (if err# (reject# err#)
                         (resolve# result#)))))))