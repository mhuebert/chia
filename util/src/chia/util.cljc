(ns chia.util
  (:require #?(:cljs [chia.util.js-interop])
            [chia.util.macros :as m]
            [chia.util.js-interop :as j])
  #?(:cljs (:require-macros [chia.util])))

(defn guard [x f]
  (when (f x)
    x))

(defn some-str [s]
  (when (and s (string? s) (not= s ""))
    s))

;; from https://github.com/clojure/core.incubator/blob/master/src/main/clojure/clojure/core/incubator.clj
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

;; modified from https://github.com/clojure/core.incubator/blob/master/src/main/clojure/clojure/core/incubator.clj
(defn disj-in
  "Dis[join]'s `value` from set at `path` returning a new nested structure.
   The set, if empty, and any empty maps that result, will not be present in the new structure."
  [m [k & ks :as path] value]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (disj-in nextmap ks value)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (let [new-set (disj (get m k) value)]
      (if (empty? new-set)
        (dissoc m k)
        (assoc m k new-set)))))

(defn update-keys [f m]
  (persistent!
   (reduce-kv (fn [m k v] (assoc! m (f k) v)) (transient (empty m)) m)))

(defn update-some-keys [m ks f]
  (reduce (fn [m k]
            (cond-> m
                    (contains? m k) (assoc k (f (get m k))))) m ks))

(defn update-vals [f m]
  (persistent!
   (reduce-kv (fn [m k v] (assoc! m k (f v))) (transient (empty m)) m)))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn apply-if-fn [f & args]
  (if (fn? f)
    (apply f args)
    f))


(m/defmacro for-map [& body]
  `(->> (for ~@body)
        (apply merge)))

(m/defmacro doto->>
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

(m/macro-time
 (defn munged-key [k]
   (munge (str (when (keyword? k)
                 (some-> (namespace k)
                         (str "__")))
               (name k)))))

(m/defmacro memoized-on [o k & body]
  (let [k (munged-key k)]
    `(or (~'chia.util.js-interop/get ~o ~k)
         (doto->> (do ~@body)
                  (~'chia.util.js-interop/assoc! ~o ~k)))))

(defn user-bindings
  "Returns all user-assigned bindings resulting from a let binding."
  [let-bindings]
  (let [;; set of symbols that will be bound via clojure.core/destructure
        bound-sym? (m/case :clj (->> (clojure.core/destructure let-bindings) ;; not selfhost-compatible
                                     (partition 2)
                                     (map first)
                                     (set))
                           :cljs symbol?)
        ;; all keywords & symbols that appear in the user destructuring side
        user-syms-keywords (->> (partition 2 let-bindings)
                                (map first)
                                (tree-seq coll? seq)
                                (group-by #(cond (symbol? %) :symbols
                                                 (keyword? %) :keywords
                                                 :else nil))
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