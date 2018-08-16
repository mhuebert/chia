(ns chia.util
  (:refer-clojure :exclude [cond])
  (:require [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [better-cond.core :as b]))

(defmacro for-map [& body]
  `(->> (for ~@body)
        (apply merge)))

(defmacro memoized-on [o k & body]
  `(or (~'chia.util.js-interop/get ~o ~k)
       (let [value# (do ~@body)]
         (~'chia.util.js-interop/assoc! ~o ~k value#)
         value#)))

(defn user-bindings
  "Returns all user-assigned bindings resulting from a let binding."
  [let-bindings]
  (let [;; set of symbols that will be bound via clojure.core/destructure
        bound-sym? (->> (destructure let-bindings)
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
       (~'js/console.group ~(str file "#" line))
       ~@(for [user-binding (user-bindings bindings)]
           `(~'js/console.log (quote ~user-binding) ~(symbol (name user-binding))))
       (~'js/console.groupEnd)
       ~@body)))

(defmacro cond [& args]
  `(better-cond.core/cond ~@args))