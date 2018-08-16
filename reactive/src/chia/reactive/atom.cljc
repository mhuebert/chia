(ns chia.reactive.atom
  "Utilities for reactively reading atoms, with key/path granularity."
  (:refer-clojure :exclude [get get-in deref select-keys assoc! dissoc! contains? swap! reset!])
  (:require [chia.reactive :as r]
            [clojure.set :as set]
            [clojure.core :as core]
            [chia.util :as u]))

(defonce ^{:doc "Index of path-based dependencies, per atom."}
         readers-by-path
         (volatile! {}))

(def ^:dynamic *write-paths*
  "A nested map representing "
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
  "Reactively dereferences any atom, creates a dependency on it for the current reader (ie. chia.reactive/*reader*)"
  [ref]
  (r/update-source-log! ref conj-set [::value])
  (core/deref ref))

(defn get
  "Like Clojure's `get`, creates dependency on key `k`."
  ([ref k]
   (r/update-source-log! ref conj-set [k ::value])
   (core/get (core/deref ref) k))
  ([ref k not-found]
   (r/update-source-log! ref conj-set [k ::value])
   (core/get (core/deref ref) k not-found)))

(defn get-in
  "Like Clojure's `get-in`, creates dependency on `path`."
  ([ref path]
   (r/update-source-log! ref conj-set (conj path ::value))
   (core/get-in (core/deref ref) path))
  ([ref path not-found]
   (r/update-source-log! ref conj-set (conj path ::value))
   (core/get-in (core/deref ref) path not-found)))

(defn apply-in
  "Applies `f` to value at `path` followed by `args`. Creates dependency on the result of this computation."
  [ref path f & args]
  (r/update-source-log! ref conj-set (into path [::fns [f args]]))
  (apply f (core/get-in (core/deref ref) path) args))

(defn keys-in
  "Returns keys at `path`, creates a dependency on the result."
  [ref path]
  (apply-in ref path keys))

(defn select-keys
  "Like Clojure's `select-keys`, creates dependency on the given keys `ks`."
  [ref ks]
  (r/update-source-log! ref into-set (mapv #(vector % ::value) ks))
  (select-keys (core/deref ref) ks))

(defn contains?
  "Like Clojure's `contains?`, creates dependency on the key `k`."
  [ref k]
  (r/update-source-log! ref conj-set [k ::value])
  (core/contains? (core/deref ref) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API - Writing Values
;;
;; The following functions mimic core Clojure functions, but operate directly on atoms (mutating them!),
;; keeping track of keys/paths so that we can efficiently update only those readers which are affected.
;;

(defn reset! [ref value]
  (core/reset! ref value))

(defn swap! [ref f & args]
  (core/reset! ref (apply f (core/deref ref) f args)))

(defn assoc!
  "Like Clojure's `assoc`, but mutates an atom, limiting reactive dependency comparisons to the modified keys."
  [ref k v & kvs]
  (let [paths (reduce (fn [m k] (assoc m k nil)) {k nil} (take-nth 2 kvs))
        ret (apply assoc (core/deref ref) k v kvs)]
    (binding [*write-paths* paths]
      (core/reset! ref ret))))

(defn assoc-in!
  "Like Clojure's `assoc-in`, but mutates an atom, limiting reactive dependency comparisons to the supplied `path`."
  [ref path value]
  (binding [*write-paths* (assoc-in {} path nil)]
    (core/swap! ref assoc-in path value)))

(defn dissoc!
  "Like Clojure's `dissoc`, but mutates an atom, limiting reactive dependency comparisons to the supplied key `k`."
  [ref k]
  (binding [*write-paths* {k nil}]
    (core/swap! ref dissoc k)))

(defn dissoc-in!
  "Dissoc keys from ref, limiting reactive dependency comparisons to the supplied `path`"
  [ref path]
  (binding [*write-paths* (assoc-in {} path nil)]
    (core/swap! ref u/dissoc-in path)))

(defn update!
  "Like Clojure's `update`, but mutates an atom, limiting reactive dependency comparisons to the supplied key `k`."
  [ref k f & args]
  (assoc! ref k (apply f (core/get (core/deref ref) k) args)))

(defn update-in!
  "Like Clojure's `update-in`, but mutates an atom, limiting reactive dependency comparisons to the supplied `path`."
  [ref path f & args]
  (assoc-in! ref path (apply f (core/get-in (core/deref ref) path) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implementation
;;

(defn readers-at-path [{value-readers ::value
                        fn-readers ::fns
                        :or {value-readers #{}}} oldval newval]
  (-> value-readers
      (cond-> fn-readers
              (as-> found (reduce-kv (fn [found [f args] readers]
                                       (cond-> found
                                               (not= (apply f oldval args) (apply f newval args))
                                               (into readers))) found fn-readers)))))

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

(defn- handle-atom-reset
  "When a watched atom changes, invalidate readers at changed paths, constrained by the current *write-path*."
  [_ source oldval newval]
  (let [readers (if *write-paths*
                  (invalidated-readers-at-write-paths *write-paths*
                                                      (core/get (core/deref readers-by-path) source)
                                                      oldval
                                                      newval)
                  (invalidated-readers (core/get (core/deref readers-by-path) source)
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
      (vswap! readers-by-path update source
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