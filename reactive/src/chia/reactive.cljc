(ns chia.reactive
  "Central point where reactivity is coordinated"
  (:require [chia.util.macros :as m])
  #?(:cljs (:require-macros [chia.reactive :as r])))

(def ^:dynamic *reader*
  "The currently-computing reader"
  nil)

(def ^:dynamic *silent*
  "Prevents changes from triggering recomputation"
  false)

(def ^:dynamic *reader-dependency-log*
  "Keeps track of what data sources a reader accesses during compute"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dependency graph, with edge data
;;

(defonce ^{:doc "{<source> {<reader> <pattern>}}"}
         dependents
         (volatile! {}))

(defonce ^{:doc "{<reader> {<source> <pattern>}}"}
         dependencies
         (volatile! {}))

(defn add-dependent [dependents source reader edge-data]
  (assoc-in dependents [source reader] edge-data))

(defn remove-dependent [dependents source reader]
  (if (> (count (get dependents source)) 1)
    (update dependents source dissoc reader)
    (dissoc dependents source)))

(defn add-dependency [dependencies reader source edge-data]
  (assoc-in dependencies [reader source] edge-data))

(defn remove-dependency [dependencies reader source]
  (if (> (count (get dependencies reader)) 1)
    (update dependencies reader dissoc source)
    (dissoc dependencies reader)))

(defn add-edge! [source reader edge-data]
  (vswap! dependents add-dependent source reader edge-data)
  (vswap! dependencies add-dependency reader source edge-data)
  edge-data)

(defn remove-edge! [source reader]
  (vswap! dependents remove-dependent source reader)
  (vswap! dependencies remove-dependency reader source)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reactive readers
;;

(defprotocol IReadReactively
  (-invalidate! [reader info]
    "Recomputes the value of a reader"))

(defn invalidate! [reader info]
  (when-not *silent*
    (if (satisfies? IReadReactively reader)
      (-invalidate! reader info)
      (reader info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reactive data sources
;;

(defprotocol IWatchableByPattern
  "Protocol which enables a reactive data source to support pattern-based dependencies."
  (update-pattern-watches! [source reader prev-patterns next-patterns source-transition]
    "a reactive `source` should implement this function to update internal indexes/listeners.

     `next-patterns` will be nil, or the result of successive applications of `log-pattern-update!` during a compute cycle.
     `prev-patterns` will be nil, or the return value of the previous call to `update-pattern-watches!`.

     The return value of this function is important -- it is what you will receive as `prev-patterns` after the next read cycle.

     When `patterns` are invalidated, `source` should call `chia.reactive/invalidate!`, passing in `reader`."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Read functions
;;


(defn update-watch!
  "Updates watch for a source<>reader combo. Handles effectful updating of `source`."
  [source reader prev-patterns next-patterns]
  (when (not= prev-patterns next-patterns)
    (let [new-source? (empty? (get @dependents source))]
      (if (empty? next-patterns)
        (remove-edge! source reader)
        (add-edge! source reader next-patterns))
      (update-pattern-watches! source
                               reader
                               prev-patterns
                               next-patterns
                               (cond (and (not new-source?)
                                          (empty? (get @dependents source))) :removed
                                     (and new-source?
                                          (not (empty? next-patterns))) :added)))))

(defn dispose-reader!
  "Removes reader from reactive graph."
  [reader]
  (doseq [[source patterns] (get @dependencies reader)]
    (update-watch! source reader patterns nil))
  reader)

(defn update-reader-deps!
  "`next-deps` should be a map of {<source>, <patterns>}."
  [reader next-deps]
  (let [prev-deps (get @dependencies reader)]
    (doseq [source (-> (set (keys next-deps))
                       (into (keys prev-deps)))]
      (update-watch! source reader (get prev-deps source) (get next-deps source)))
    reader))


(m/defmacro with-dependency-log
  "Evaluates `body`, returns value and logged dependencies.

   `reader` must satisfy IReadReactively."
  [reader & body]
  `(binding [*reader* ~reader
             *reader-dependency-log* (volatile! {})]
     (let [value# (do ~@body)]
       [value# @*reader-dependency-log*])))

(m/defmacro with-dependency-tracking!
  "Evaluates `body`, creating dependencies for `reader` with arbitrary data sources."
  [reader & body]
  `(let [[value# dependencies#] (~'chia.reactive/with-dependency-log
                                 ~reader
                                 ~@body)]
     (update-reader-deps! ~reader dependencies#)
     value#))

(m/defmacro silently
  [& body]
  `(binding [*silent* true]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For implementing data sources
;;

(defn update-source-log!
  "Logs read of a pattern-supporting dependency. `source` must satisfy IWatchableByPattern.

   `f` will be called by the existing patterns for the current source/reader, and `args`."
  [source f & args]
  (when *reader*
    (vswap! *reader-dependency-log* assoc source (apply f (get @*reader-dependency-log* source) args)))
  source)
