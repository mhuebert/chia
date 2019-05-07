(ns chia.reactive
  "Central point where reactivity is coordinated"
  (:require [chia.util.macros :as m]
            [chia.util.perf :as perf]
            [applied-science.js-interop :as j])
  #?(:cljs (:require-macros [chia.reactive :as r :refer [log-read*]])))

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
(defprotocol ITransition
  (on-transition [source transition]
    "Notify `source` when it is added or removed from the reactive graph.
     - :observed means source has listeners
     - :un-observed means source does not have listeners"))

(defn transition [source kind]
  (when (satisfies? ITransition source)
    (on-transition source kind)))

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
  (when-not (contains? @dependents source) (transition source :observed))
  (vswap! dependents add-dependent source reader edge-data)
  (vswap! dependencies add-dependency reader source edge-data)
  edge-data)

(defn remove-edge! [source reader]
  (vswap! dependents remove-dependent source reader)
  (vswap! dependencies remove-dependency reader source)
  (when-not (contains? @dependents source) (transition source :un-observed))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reactive readers
;;

(defprotocol IReadReactively
  (-invalidate! [reader info]
    "We 'invalidate' a reader whenever one of its dependencies has changed.
     Implementors should ensure that a call to `invalidate` will cause the
     reader to re-evaluate."))

(defn invalidate!
  ([reader]
   (invalidate! reader nil))
  ([reader info]
   (when-not *silent*
     (if (satisfies? IReadReactively reader)
       (-invalidate! reader info)
       ;; in the simplest case, a reader is simply a function.
       (reader info)))))

(defn get-readers [source]
  (keys (get @dependents source)))

(defn invalidate-readers! [source]
  (doseq [reader (keys (get @dependents source))]
    (invalidate! reader nil))
  source)

(defn invalidate-readers-for-sources! [sources]
  (doseq [reader (into [] (comp (mapcat get-readers)
                                (distinct)) sources)]
    (invalidate! reader nil))
  sources)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reactive data sources
;;

(defprotocol ILogReadPatterns
  "Protocol which enables a reactive data source to support pattern-based dependencies."
  (update-reader-deps [source reader prev-patterns next-patterns]
    "a reactive `source` should implement this function to keep track of dependent readers.

     `next-patterns` is the result of successive applications of `log-read!` during a read.
     `prev-patterns` is for comparison with the last read.

     When `patterns` are invalidated, `source` should call `chia.reactive/invalidate!`, passing in `reader`."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Read functions
;;

(defn update-simple! [source reader prev next]
  (cond (nil? prev) (add-edge! source reader ::simple)
        (nil? next) (remove-edge! source reader)))

(defn update-watch!
  "Updates watch for a source<>reader combo. Handles effectful updating of `source`."
  [source reader prev-patterns next-patterns]
  (when (not= prev-patterns next-patterns)
    (if (or (perf/identical? ::simple prev-patterns)
            (perf/identical? ::simple next-patterns))
      (update-simple! source reader prev-patterns next-patterns)
      (do
        (if (empty? next-patterns)
          (remove-edge! source reader)
          (add-edge! source reader next-patterns))
        (update-reader-deps source
                            reader
                            prev-patterns
                            next-patterns)))))

(defn dispose-reader!
  "Removes reader from reactive graph."
  [reader]
  (doseq [[source patterns] (get @dependencies reader)]
    (update-watch! source reader patterns nil))
  reader)

(defn update-deps!
  "`next-deps` should be a map of {<source>, <patterns>}."
  [reader next-deps]
  {:pre [reader]}
  (let [prev-deps (get @dependencies reader)]
    (doseq [source (-> (set (keys next-deps))
                       (into (keys prev-deps)))]
      (update-watch! source
                     reader
                     (get prev-deps source)
                     (get next-deps source)))
    reader))

(m/defmacro with-dependency-log
  "Evaluates `body`, returns value and logged dependencies.

   `reader` must satisfy IReadReactively."
  [reader & body]
  `(binding [*reader* ~reader
             *reader-dependency-log* (volatile! {})]
     (let [value# (do ~@body)]
       (j/obj .-value value#
              .-deps @*reader-dependency-log*))))

(m/defmacro with-dependency-tracking!
  "Evaluates `body`, creating dependencies for `reader` with arbitrary data sources."
  [{:as   options
    :keys [schedule
           reader]
    :or   {schedule '.call}} & body]
  {:pre [(map? options)]}
  `(let [reader# ~reader
         result# (~'chia.reactive/with-dependency-log reader# ~@body)]
     (~schedule #(update-deps! reader# (.-deps result#)))
     (.-value result#)))

(m/defmacro silently
  [& body]
  `(binding [*silent* true]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For implementing data sources
;;

(m/defmacro ^:private log-read* [source expr]
  `(do (when (some? *reader*)
         (vswap! *reader-dependency-log* assoc ~source ~expr))
       ~source))

(m/defmacro log-read!
  ([source]
   `(log-read* ~source ::simple))
  ([source f & args]
   `(log-read* ~source (~f (get @*reader-dependency-log* ~source) ~@args))))
