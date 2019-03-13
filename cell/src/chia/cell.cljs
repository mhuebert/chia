(ns chia.cell
  (:require [chia.cell.util :as util :refer [id]]
            [chia.cell.runtime :as runtime]
            [com.stuartsierra.dependency :as dep]
            [chia.cell.deps :as deps]
            [chia.reactive :as r])
  (:require-macros [chia.cell]))

(def ^:dynamic *graph* (atom (new deps/CellGraph {})))

(defn get-function [id]
  (get-in @*graph* [:cells id :function]))

(defn set-function! [id f]
  (swap! *graph* assoc-in [:cells id :function] f))

(defn get-value [id]
  (get-in @*graph* [:cells id :value]))

(defn get-global-meta [id]
  (get-in @*graph* [:cells id :meta]))

;;;;;;;;;;;;;;;;;;
;;
;; Dynamic state

(def ^:dynamic *stack*
  "Stack of currently evaluating cells"
  (list))

(def ^:dynamic ^:private *read-log*
  "Track cell dependencies during eval"
  nil)

(def ^:dynamic ^:private *change-log*
  "Track cell changes during eval"
  nil)

(defn log-read! [cell]
  (when *read-log*
    (vswap! *read-log* conj cell))
  (r/log-read! cell))

;;;;;;;;;;;;;;;;;;
;;
;; Cell evaluation

(defn- invalidate-readers! [cell]
  (if *change-log*
    (vswap! *change-log* conj cell)
    (r/invalidate-readers! cell)))

(declare Cell)

(defn- clean-eval [cell]
  (when-let [f (get-function (id cell))]
    (try (f cell)
         (catch js/Error e
           (runtime/dispose! cell)
           (throw e)))))

(defn- eval-cell [graph cell]
  (if (= cell (first *stack*))
    graph
    (binding [*graph* (atom graph)
              *stack* (cons cell *stack*)
              *read-log* (volatile! #{})
              runtime/*runtime* (::runtime (get-global-meta (id cell)))]
      (runtime/dispose! cell)
      (let [value (clean-eval cell)
            next-deps (disj @*read-log* cell)]
        (-reset! cell value)
        (-> @*graph*
            (deps/transition-deps cell next-deps))))))

(defn- eval-cell! [cell]
  (reset! *graph* (eval-cell @*graph* cell))
  cell)

(def ^:dynamic ^:private *computing-dependents* false)

(defn- eval-cell-and-dependents [graph cell]
  (if *computing-dependents*
    graph
    (binding [*computing-dependents* true]
      (let [sorted-cells (deps/transitive-dependents-sorted graph cell)]
        (reduce eval-cell graph sorted-cells)))))

(defn- eval-cell-and-dependents! [cell]
  (when-not *computing-dependents*
    (binding [*change-log* (volatile! [])]
      (try
        (reset! *graph* (eval-cell-and-dependents @*graph* cell))
        (finally
         (r/invalidate-readers-for-sources! @*change-log*)))))
  cell)

;;;;;;;;;;;;;;;;;;
;;
;; Cell views

(defn status-view
  "Experimental: cells that implement IStatus can 'show' themselves differently depending on status."
  [this]
  (cond
    (:async/loading? this) ^:hiccup [:.cell-status
                                     [:.circle-loading
                                      [:div]
                                      [:div]]]

    (:async/error this) ^:hiccup [:div.pa3.bg-darken-red.br2
                                  (or (:async/error this)
                                      [:.circle-error
                                       [:div]
                                       [:div]])]))

(defn default-view [cell]
  (if (or (:async/loading? cell)
          (:async/error cell))
    (status-view cell)
    @cell))

(defn view [cell]
  (let [view-fn (get (meta cell) :cell/view default-view)]
    (view-fn cell)))

(defn with-view
  "Attaches `view-fn` to cell"
  [cell view-fn]
  (vary-meta cell assoc :cell/view view-fn))

;;;;;;;;;;;;;;;;;;
;;
;; Cell status

(defn vary-global-meta! [cell f & args]
  (swap! *graph* update-in [:cells (id cell) :meta] #(apply f % args))
  (invalidate-readers! cell)
  (eval-cell-and-dependents! cell))

(defn loading! [cell]
  (vary-global-meta! cell merge {:async/loading? true}))

(defn error! [cell error]
  (vary-global-meta! cell merge {:async/loading? false
                                 :async/error error}))
(defn complete! [cell]
  (vary-global-meta! cell merge {:async/loading? false
                                 :async/error nil}))

(defn- transitive
  "Recursively expands the set of dependency relationships starting
  at (get neighbors x), for each x in node-set"
  [k cells node-set]
  (loop [unexpanded (mapcat (comp k cells id) node-set)
         expanded #{}]
    (if-let [[node & more] (seq unexpanded)]
      (if (contains? expanded node)
        (recur more expanded)
        (recur (concat more (get-in cells [(id node) k]))
               (conj expanded node)))
      expanded)))

;;;;;;;;;;;;;;;;;;
;;
;; Cell type

(deftype Cell [id ^:mutable meta dependencies dependents]

  IEquiv
  (-equiv [this other]
    (and (instance? Cell other)
         (keyword-identical? id (.-id other))))

  IHash
  (-hash [this]
    (hash id))

  IWithMeta
  (-with-meta [this m]
    (Cell. id m))

  IMeta
  (-meta [_] meta)

  IPrintWithWriter
  (-pr-writer [this writer _] (write-all writer (str "⚪️" id)))

  runtime/IDispose
  (on-dispose [this f]
    (swap! *graph* update-in [:cells id :meta ::on-dispose] conj f))
  (-dispose! [this]
    (doseq [f (::on-dispose (get-global-meta id))]
      (f))
    (swap! *graph* update-in [:cells id :meta] dissoc ::on-dispose)
    this)

  ;; Atom behaviour

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [f (vals (::watches (get-global-meta id)))]
      (f this oldval newval)))
  (-add-watch [this key f]
    (swap! *graph* update-in [:cells id :meta ::watches] assoc key f))
  (-remove-watch [this key]
    (swap! *graph* update-in [:cells id :meta ::watches] dissoc key))

  IDeref
  (-deref [this]
    (log-read! this)
    (get-value id))

  IReset
  (-reset! [this newval]
    (let [oldval (get-value id)]
      (swap! *graph* assoc-in [:cells id :value] newval)
      (-notify-watches this oldval newval)
      (invalidate-readers! this)
      (eval-cell-and-dependents! this)
      newval))

  ISwap
  (-swap! [this f] (-reset! this (f (get-value id))))
  (-swap! [this f a] (-reset! this (f (get-value id) a)))
  (-swap! [this f a b] (-reset! this (f (get-value id) a b)))
  (-swap! [this f a b xs] (-reset! this (apply f (get-value id) a b xs)))

  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (r/log-read! this)
    (case k
      (:async/loading?
       :async/error) (get (get-global-meta id) k)
      :async/value (get-value id)
      (throw (js/Error. (str "Unknown key on cell: " k)))))



  dep/DependencyGraph
  (immediate-dependencies [cell node]
    (get-in cells [(id node) :dependencies] #{}))
  (immediate-dependents [graph node]
    (assert node "cell must exist")
    (get-in cells [(id node) :dependents] #{}))
  (transitive-dependencies [graph node]
    (transitive :dependencies cells #{node}))
  (transitive-dependencies-set [graph node-set]
    (transitive :dependencies cells node-set))
  (transitive-dependents [graph node]
    (transitive :dependents cells #{node}))
  (transitive-dependents-set [graph node-set]
    (transitive :dependents cells node-set))
  (nodes [graph]
    (some->> cells
             (remove (fn [[_ d]] (:instance d)))
             (seq)
             (prn ::missing-NODES))
    (->> (vals cells)
         (mapv :instance)
         (set)))

  dep/DependencyGraphUpdate
  (depend [graph node dep-node]
    (when (or (= node dep-node) (dep/depends? graph dep-node node))
      (throw (ex-info (str "Circular dependency between "
                           (pr-str node) " and " (pr-str dep-node))
                      {:reason ::circular-dependency
                       :node node
                       :dependency dep-node})))
    (CellGraph. (-> cells
                    (update-in [(id node) :dependencies] dep/set-conj dep-node)
                    (update-in [(id dep-node) :dependents] dep/set-conj node))))
  (remove-edge [graph node dep-node]
    (CellGraph. (-> cells
                    (update-in [(id node) :dependencies] disj dep-node)
                    (update-in [(id dep-node) :dependents] disj node))))
  (remove-all [graph node]
    (CellGraph. (let [{:keys [dependents
                              dependencies]} (get cells (id node))]
                  (as-> cells cells
                        (dissoc cells (id node))
                        (reduce (fn [cells d]
                                  (update-in cells [(id d) :dependencies] disj node)) cells dependents)
                        (reduce (fn [cells d]
                                  (update-in cells [(id d) :dependents] disj node)) cells dependencies)))))
  (remove-node [graph node]
    (CellGraph. (dissoc cells (id node))))
  )

;;;;;;;;;;;;;;;;;;
;;
;; Cell type

(defn purge-cell! [cell]
  (runtime/-dispose! cell)
  (swap! *graph* dep/remove-node cell))

(defn cell*
  "Returns a new cell, or an existing cell if `id` has been seen before.
  `f` should be a function that, given the cell's previous value, returns its next value.
  `state` is not for public use."
  ([f]
   (cell* (keyword "chia.cell.temp" (str "_" (util/unique-id))) f nil))
  ([id f]
   (cell* id f nil))
  ([id f {::keys [reload
                  prev-cell
                  def?]}]
   (cond reload (do (set-function! id f)
                    (eval-cell! prev-cell))
         (contains? (:cells @*graph*) id) (get-in @*graph* [:cells id :instance])
         :else (let [cell (Cell. id nil)]
                 (swap! *graph* (fn [graph]
                                  (assoc-in graph [:cells id] {:instance cell
                                                               :function f
                                                               :meta {::runtime runtime/*runtime*
                                                                      ::def? def?}})))
                 (when-not def?
                   (runtime/on-dispose runtime/*runtime* #(purge-cell! cell)))
                 (eval-cell! cell)))))

(defn cell
  [{:keys [key]} value]
  (assert key "Cells created by functions require a :key")
  (let [[ns prefix] (if-let [parent-id (some-> (first *stack*)
                                               (id))]
                      [(namespace parent-id) (name parent-id)
                       "chia.cell.inline" "_"])
        id (keyword ns (str prefix "#" (hash key)))]
    (cell* id (constantly value))))