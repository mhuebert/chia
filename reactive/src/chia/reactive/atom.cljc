(ns chia.reactive.atom
  "Utilities for reactively reading atoms, with key/path granularity."
  (:refer-clojure :exclude [get get-in deref select-keys assoc! dissoc! contains? swap! reset!])
  (:require [chia.reactive :as r]
            [clojure.set :as set]
            [clojure.core :as core]
            [chia.util :as u]))

(defonce ^{:doc "Trie containing active readers within a path (per atom)"}
         listeners-by-path
         (volatile! {}))

(def ^:dynamic *write-paths*
  "Trie representing paths that may be changed by a mutation"
  nil)

(def conj-set (fnil conj #{}))
(def into-set (fnil into #{}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API - Reading Values
;;
;; The following functions take an atom (`ref`) as their first argument. Reads are logged so that
;; views can depend on specific keys/paths of an atom, and only re-render when those keys/paths change.
;;

(defn deref
  "Reactively dereferences `ref`, creates a dependency for the current reader"
  [ref]
  (r/log-read! ref conj-set [::value])
  (core/deref ref))

(defn get
  "Like Clojure `get`, creates dependency on key `k`."
  ([ref k]
   (r/log-read! ref conj-set [k ::value])
   (core/get (core/deref ref) k))
  ([ref k not-found]
   (r/log-read! ref conj-set [k ::value])
   (core/get (core/deref ref) k not-found)))

(defn get-in
  "Like Clojure `get-in`, creates dependency on `path`."
  ([ref path]
   (r/log-read! ref conj-set (conj path ::value))
   (core/get-in (core/deref ref) path))
  ([ref path not-found]
   (r/log-read! ref conj-set (conj path ::value))
   (core/get-in (core/deref ref) path not-found)))

(defn apply-in
  "Applies `f` to value at `path` followed by `args`. Creates dependency on the result of this computation."
  [ref path f & args]
  (r/log-read! ref conj-set (into path [::fns [f args]]))
  (apply f (core/get-in (core/deref ref) path) args))

(defn keys-in
  "Returns keys at `path`, creates a dependency on the result."
  [ref path]
  (apply-in ref path keys))

(defn select-keys
  "Like Clojure's `select-keys`, creates dependency on the given keys `ks`."
  [ref ks]
  (r/log-read! ref into-set (mapv #(vector % ::value) ks))
  (select-keys (core/deref ref) ks))

(defn contains?
  "Like Clojure's `contains?`, creates dependency on the key `k`."
  [ref k]
  (r/log-read! ref conj-set [k ::value])
  (core/contains? (core/deref ref) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API - Writing Values
;;
;; The following functions mimic core Clojure functions, but operate directly on atoms (mutating them!),
;; keeping track of keys/paths so that we can efficiently update dependent readers.
;;

(defn reset! [ref value]
  (core/reset! ref value))

(defn swap! [ref f & args]
  (core/reset! ref (apply f (core/deref ref) f args)))

(defn assoc!
  "Like Clojure `assoc`, but mutates `ref`, limiting reactive dependency comparisons to the modified keys."
  [ref k v & kvs]
  (let [paths (reduce (fn [m k] (assoc m k nil)) {k nil} (take-nth 2 kvs))
        ret (apply assoc (core/deref ref) k v kvs)]
    (binding [*write-paths* paths]
      (core/reset! ref ret))))

(defn assoc-in!
  "Like Clojure `assoc-in`, but mutates `ref`, limiting reactive dependency comparisons to the supplied `path`."
  [ref path value]
  (binding [*write-paths* (assoc-in {} path nil)]
    (core/swap! ref assoc-in path value)))

(defn dissoc!
  "Like Clojure `dissoc`, but mutates `ref`, limiting reactive dependency comparisons to the supplied key `k`."
  [ref k]
  (binding [*write-paths* {k nil}]
    (core/swap! ref dissoc k)))

(defn dissoc-in!
  "Dissoc keys from ref, limiting reactive dependency comparisons to the supplied `path`"
  [ref path]
  (binding [*write-paths* (assoc-in {} path nil)]
    (core/swap! ref u/dissoc-in path)))

(defn update!
  "Like Clojure `update`, but mutates `ref`, limiting reactive dependency comparisons to the supplied key `k`."
  [ref k f & args]
  (assoc! ref k (apply f (core/get (core/deref ref) k) args)))

(defn update-in!
  "Like Clojure `update-in`, but mutates `ref`, limiting reactive dependency comparisons to the supplied `path`."
  [ref path f & args]
  (assoc-in! ref path (apply f (core/get-in (core/deref ref) path) args)))

(defn merge! [ref m]
  (binding [*write-paths* (reduce-kv #(assoc %1 %2 nil) {} m)]
    (core/swap! ref merge m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implementation
;;

(defn readers-at-path [{:as reader-index
                        value-readers ::value
                        fn-readers ::fns
                        :or {value-readers #{}}} oldval newval]
  (if-not fn-readers
    value-readers
    (reduce-kv (fn [found [f args] readers]
                 (cond-> found
                         (not= (apply f oldval args) (apply f newval args))
                         (into readers))) value-readers fn-readers)))

(defn- invalidated-readers
  "Traverses `reader-index`, returning readers at paths which differ between `oldval` and `newval`."
  ([reader-index oldval newval]
   (when (not= oldval newval)
     (-> (readers-at-path reader-index oldval newval)
         (into (mapcat (fn [[k v]]
                         (invalidated-readers v
                                              (core/get oldval k)
                                              (core/get newval k))))
               (dissoc reader-index ::value ::fns))))))

(defn- invalidated-readers-at-write-paths
  "Like `invalidated-readers`, but scoped to particular `write-paths`."
  [write-paths reader-index oldval newval]
  (when (not= oldval newval)
    (cond-> (readers-at-path reader-index oldval newval)
            (some? write-paths)
            (into (mapcat (fn [[k write-paths]]
                            (when (core/contains? reader-index k)
                              (invalidated-readers-at-write-paths write-paths
                                                                  (core/get reader-index k)
                                                                  (core/get oldval k)
                                                                  (core/get newval k)))))
                  write-paths))))

(defn invalidate-path! [source path]
  (let [index (get @listeners-by-path source)
        {value-readers ::value
         fn-readers ::fns} (core/get-in index path)]
    (doseq [reader (into value-readers (vals fn-readers))]
      (r/invalidate! reader {}))))

(defn- handle-atom-reset
  "When a watched atom changes, invalidate readers at changed paths, constrained by the current *write-path*."
  [_ source oldval newval]
  (let [readers (if *write-paths*
                  (invalidated-readers-at-write-paths *write-paths*
                                                      (core/get (core/deref listeners-by-path) source)
                                                      oldval
                                                      newval)
                  (invalidated-readers (core/get (core/deref listeners-by-path) source)
                                       oldval
                                       newval))]
    (doseq [reader readers]
      (r/invalidate! reader {::before oldval
                             ::after newval}))))

(defonce ^{:doc "A set of readers for each atom, used to manage setup/teardown of watches."
           :private true}
         readers
         (volatile! {}))

(extend-type Atom
  r/IWatchableByPattern
  (r/update-pattern-watches! [source reader prev-patterns next-patterns source-transition]
   ;; update nested index of paths->readers
    (let [added (set/difference next-patterns prev-patterns)
          removed (set/difference prev-patterns next-patterns)]
      (vswap! listeners-by-path update source
              (fn [index]
                (as-> index index
                      (reduce (fn [m path]
                                (update-in m path conj-set reader)) index added)
                      (reduce (fn [m path]
                                (u/disj-in m path reader)) index removed)))))

    (case source-transition
      :added (add-watch source :chia.reactive/atom handle-atom-reset)
      :removed (remove-watch source :chia.reactive/atom)
      nil)

    (if (empty? next-patterns)
      (vswap! readers u/disj-in [source] reader)
      (vswap! readers update source conj-set reader))

    next-patterns))



(comment

 (assoc (atom {}) :a 1 :b 2)

 (invalidated-readers-at-write-paths
  {:a {:b {:c true}}}
  {::readers #{:ROOT}
   :greeting {::readers #{:G}}
   :a {::readers #{:A}
       1 {::readers #{:A-1}}
       2 {::readers #{:A-2}}}}
  {:greeting "Hello"
   :a [0 1 2 3]}
  {:greeting "Hellox"
   :a [0 1 2]}))