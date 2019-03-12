(ns chia.cell
  (:require [chia.cell.util :as util :refer [id]]
            [chia.cell.runtime :as runtime]
            [com.stuartsierra.dependency :as dep]
            [chia.cell.deps :as deps]
            [chia.reactive :as r]
            [chia.view :as v]
            [chia.view.registry :as registry])
  (:require-macros [chia.cell]))

(def ^:dynamic *graph* (atom (new deps/CellGraph {} {} {})))

(defn get-value [id]
  (get-in @*graph* [:cells id :value]))

(defn get-instance [id]
  (get-in @*graph* [:cells id :instance]))

(defn set-instance [graph id cell]
  (assoc-in graph [:cells id :instance] cell))

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
    (vswap! *read-log* conj (id cell)))
  (r/log-read! cell))

;;;;;;;;;;;;;;;;;;
;;
;; Cell evaluation

(defn- invalidate-readers! [cell]
  (if *change-log*
    (vswap! *change-log* conj cell)
    (r/invalidate-readers! cell)))

(defn- clean-eval [cell]
  (try (.computeNext cell cell)
       (catch js/Error e
         (runtime/dispose! cell)
         (throw e))))

(defn- eval-cell [graph cell]
  (if (= cell (first *stack*))
    graph
    (binding [*graph* (atom graph)
              *stack* (cons cell *stack*)
              *read-log* (volatile! #{})
              runtime/*runtime* (:runtime (.-internalState cell))]
      (runtime/dispose! cell)
      (let [value (clean-eval cell)
            next-deps (disj @*read-log* (id cell))]
        (-reset! cell value)
        (-> @*graph*
            (deps/transition-deps (id cell) next-deps))))))

(defn- eval-cell! [cell]
  (reset! *graph* (eval-cell @*graph* cell))
  cell)

(def ^:dynamic ^:private *computing-dependents* false)

(defn- eval-cell-and-dependents [graph cell]
  (if *computing-dependents*
    graph
    (binding [*computing-dependents* true]
      (let [sorted-cells (->> (deps/transitive-dependents-sorted graph (id cell))
                              (keep get-instance))]
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

(defprotocol IMetaReactive
  (merge-meta! [this _meta]
               "Set meta, cause reactive updates."))

(defn loading! [cell]
  (merge-meta! cell {:async/loading? true}))
(defn error! [cell error]
  (merge-meta! cell {:async/loading? false
                     :async/error error}))
(defn complete! [cell]
  (merge-meta! cell {:async/loading? false
                     :async/error nil}))

;;;;;;;;;;;;;;;;;;
;;
;; Cell type

(deftype Cell
  [id computeNext ^:mutable internalState ^:mutable meta]

  IWithMeta
  (-with-meta [this m]
    (set! meta m)
    this)

  IMeta
  (-meta [_] meta)

  IMetaReactive
  (merge-meta! [this m]
    (set! meta (merge meta m))
    (invalidate-readers! this)
    (eval-cell-and-dependents! this))

  IPrintWithWriter
  (-pr-writer [this writer _]
    (write-all writer (str "⚪️" id)))

  runtime/IDispose
  (on-dispose [this f]
    (set! internalState (update internalState :on-dispose conj f)))
  (-dispose! [this]
    (doseq [f (get internalState :on-dispose)]
      (f))
    (set! internalState (dissoc internalState :on-dispose))
    this)

  ;; Atom behaviour

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [f (vals (:watches internalState))]
      (f this oldval newval)))
  (-add-watch [this key f]
    (set! internalState (update internalState :watches assoc key f)))
  (-remove-watch [this key]
    (set! internalState (update internalState :watches dissoc key)))

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
       :async/error) (get meta k)
      :async/value (get-value id)
      (throw (js/Error. (str "Unknown key on cell: " k))))))

;;;;;;;;;;;;;;;;;;
;;
;; Cell type

(defn purge-cell! [id]
  (let [cell (get-instance id)]
    (runtime/-dispose! cell))
  (swap! *graph* dep/remove-node id))

(defn cell*
  "Returns a new cell, or an existing cell if `id` has been seen before.
  `f` should be a function that, given the cell's previous value, returns its next value.
  `state` is not for public use."
  ([f]
   (cell* (keyword "chia.cell.temp" (str "_" (util/unique-id))) f))
  ([id f]
   (or (get-in @*graph* [:cells id :instance])
       (let [cell (Cell. id f {:runtime runtime/*runtime*} nil)]
         (runtime/on-dispose runtime/*runtime* #(purge-cell! id))

         ;; if cell is created in a view context, dispose of it when
         ;; that view unmounts.
         (some-> registry/*view*
                 (v/on-unmount! ::cell #(runtime/dispose! (.-runtime cell))))

         (swap! *graph* set-instance id cell)
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