(ns chia.cell
  (:refer-clojure :exclude [eval])
  (:require [chia.cell.util :as util]
            [chia.util :as u]
            [chia.cell.runtime :as runtime]
            [chia.reactive :as r]
            [applied-science.js-interop :as j]
            [goog.object :as gobj]
            [clojure.set :as set])
  (:require-macros [chia.cell :as c]))

;;;;;;;;;;;;;;;;;;
;;
;; Dynamic state

(def ^:dynamic *stack*
  "Stack of currently evaluating cells"
  (list))

(def ^:dynamic ^:private *read-log*
  "Track cell dependencies during eval"
  nil)

(def ^:dynamic *tx-log*
  "Track cell changes during tx"
  nil)

(defn- mutate-cell! [cell new-attrs-js]
  (assert cell "Cannot mutate a nothing cell")
  (when-not new-attrs-js
    (throw (js/Error. "no new attrs")))
  (if *tx-log*
    (vswap! *tx-log* update cell #(doto (or % #js{})
                                    (gobj/extend new-attrs-js)))
    (gobj/extend cell new-attrs-js)))

(defn- tx-cell [cell]
  (some-> *tx-log* (deref) (get cell)))

(defn- notify-cell-changed! [cell oldval newval]
  (r/invalidate-readers! cell)
  (-notify-watches cell oldval newval))

(defn- tx! [f]
  (let [[value tx-changed]
        (binding [*tx-log* (volatile! {})]
          (let [value (f)]
            [value @*tx-log*]))]
    (doseq [[cell changes] tx-changed]
      (mutate-cell! cell changes)
      (let [newval (.-value changes)
            oldval (.-value cell)]
        (when (not= oldval newval)
          (notify-cell-changed! cell oldval newval))))
    value))

(defn internal-state [cell]
  (c/read-tx-key cell .-internal))

(defn log-read! [cell]
  (when *read-log*
    (vswap! *read-log* conj cell))
  (r/log-read! cell)
  cell)

;;;;;;;;;;;;;;;;;;
;;
;; Dependency graph

(def set-conj (fnil conj #{}))

(defn immediate-dependencies [cell]
  (.-dependencies cell))

(defn immediate-dependents [cell]
  (.-dependents cell))

(defn transitive-sorted [f]
  (fn -transitive-sorted
    ([cell]
     (->> cell
          (-transitive-sorted [#{cell} []])
          (second)))
    ([[seen results] cell]
     (let [new (set/difference (f cell) seen)]
       (reduce -transitive-sorted
               [(into (conj seen cell) new)
                (-> results
                    (cond-> (not (seen cell)) (conj cell))
                    (into new))]
               new)))))

(def transitive-dependents (transitive-sorted immediate-dependents))
(def transitive-dependencies (transitive-sorted immediate-dependencies))

(defn depend! [cell dep]
  (c/assoc-tx-key! cell .-dependencies
                   (set-conj (immediate-dependencies cell) dep))
  (c/assoc-tx-key! dep .-dependents
                   (set-conj (immediate-dependents dep) cell)))

(defn remove-edge! [cell dep]
  (c/assoc-tx-key! cell .-dependencies
                   (disj (immediate-dependencies cell) dep))
  (c/assoc-tx-key! dep .-dependents
                   (disj (immediate-dependents dep) cell)))

(defn transition-deps! [cell next-dependency-nodes]
  (let [prev-dependencies (c/read-tx-key cell .-dependencies)]
    (doseq [added (set/difference next-dependency-nodes prev-dependencies)]
      (depend! cell added))
    (doseq [removed (set/difference prev-dependencies next-dependency-nodes)]
      (remove-edge! cell removed))
    nil))

;;;;;;;;;;;;;;;;;;
;;
;; Cell evaluation

(defn- eval [cell]
  (let [f (c/read-tx-key cell .-f)
        value (c/read-tx-key cell .-value)]
    (try (f value)
         (catch js/Error e
           (runtime/dispose! tx-cell)
           (throw e)))))

(defn- eval-and-set! [cell]
  (if (= cell (first *stack*))
    cell
    (binding [*stack* (cons cell *stack*)
              *read-log* (volatile! #{})
              runtime/*runtime* (::runtime (internal-state cell))]
      (runtime/dispose! cell)
      (let [value (eval cell)
            next-deps (disj @*read-log* cell)]
        (-reset! cell value)
        (transition-deps! cell next-deps)))))

(def ^:dynamic ^:private *computing-dependents* false)

(defn- eval-with-dependents! [cell]
  (when-not *computing-dependents*
    (binding [*computing-dependents* true]
      (doseq [cell (transitive-dependents cell)]
        (eval-and-set! cell))))
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

(defn update-internal! [cell f & args]
  (c/update-tx-key! cell .-internal #(apply f % args))
  (tx! #(eval-with-dependents! cell))
  (r/invalidate-readers! cell))

(defn loading! [cell]
  (update-internal! cell merge {:async/loading? true}))

(defn error! [cell error]
  (update-internal! cell merge {:async/loading? false
                                :async/error error}))
(defn complete! [cell]
  (update-internal! cell merge {:async/loading? false
                                :async/error nil}))

;;;;;;;;;;;;;;;;;;
;;
;; Cell type

(deftype Cell [id ^:mutable f ^:mutable value ^:mutable internal ^:mutable dependencies ^:mutable dependents ^:mutable meta]

  IEquiv
  (-equiv [this other]
    (and (instance? Cell other)
         (keyword-identical? id (.-id other))))

  IHash
  (-hash [this]
    (hash id))

  IWithMeta
  (-with-meta [this m]
    (let [tx-attrs (tx-cell this)]
      (Cell. (j/get tx-attrs .-id id)
             (j/get tx-attrs .-f f)
             (j/get tx-attrs .-value value)
             (j/get tx-attrs .-internal internal)
             (j/get tx-attrs .-dependencies dependencies)
             (j/get tx-attrs .-dependents dependents)
             m)))

  IMeta
  (-meta [_] meta)

  IPrintWithWriter
  (-pr-writer [this writer _] (write-all writer (str "⚪️" id)))

  runtime/IDispose
  (on-dispose [this f]
    (c/update-tx-key! this .-internal update ::on-dispose conj f))
  (-dispose! [this]
    (doseq [f (::on-dispose (internal-state this))]
      (f))
    (c/update-tx-key! this .-internal dissoc ::on-dispose)
    this)

  ;; Atom behaviour

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [f (vals (::watches (internal-state this)))]
      (f this oldval newval)))
  (-add-watch [this key f]
    (c/update-tx-key! this .-internal update ::watches assoc key f))
  (-remove-watch [this key]
    (c/update-tx-key! this .-internal update ::watches dissoc key))

  IDeref
  (-deref [this]
    (log-read! this)
    (c/read-tx-key this .-value))

  IReset
  (-reset! [this newval]
    (tx!
     (fn []
       (mutate-cell! this (j/obj .-value newval))
       (eval-with-dependents! this)))
    newval)

  ISwap
  (-swap! [this f] (-reset! this (f (c/read-tx-key this .-value))))
  (-swap! [this f a] (-reset! this (f (c/read-tx-key this .-value) a)))
  (-swap! [this f a b] (-reset! this (f (c/read-tx-key this .-value) a b)))
  (-swap! [this f a b xs] (-reset! this (apply f (c/read-tx-key this .-value) a b xs)))

  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (r/log-read! this)
    (case k
      (:async/loading?
       :async/error) (get (internal-state id) k)
      :async/value (c/read-tx-key this .-value)
      (throw (js/Error. (str "Unknown key on cell: " k))))))

;;;;;;;;;;;;;;;;;;
;;
;; Cell construction

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
   (if reload
     (do (mutate-cell! prev-cell (j/obj .-f f))
         (tx! #(eval-and-set! prev-cell))
         prev-cell)
     (u/memoized-on runtime/*runtime* (str id)
       (let [cell (Cell. id
                         f
                         nil
                         {::runtime runtime/*runtime*
                          ::def? def?}
                         #{}
                         #{}
                         {})]
         (runtime/on-dispose runtime/*runtime* #(runtime/dispose! cell))
         (tx! #(eval-and-set! cell))
         cell)))))

(defn cell
  [{:keys [key]} value]
  (assert key "Cells created by functions require a :key")
  (let [[ns prefix] (if-let [parent-id (some-> (first *stack*)
                                               .-id)]
                      [(namespace parent-id) (name parent-id)
                       "chia.cell.inline" "_"])
        id (keyword ns (str prefix "#" (hash key)))]
    (cell* id (constantly value))))