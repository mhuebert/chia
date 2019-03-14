(ns chia.cell
  (:refer-clojure :exclude [eval])
  (:require [chia.cell.util :as util]
            [chia.view.registry :as registry]
            [chia.util :as u]
            [chia.cell.runtime :as runtime]
            [chia.reactive :as r]
            [applied-science.js-interop :as j]
            [goog.object :as gobj]
            [clojure.set :as set]
            [chia.view :as v]
            [kitchen-async.promise :as p])
  (:require-macros [chia.cell :as c]))

;;;;;;;;;;;;;;;;;;
;;
;; Dynamic state

(def ^:dynamic *cell*
  "Currently evaluating cell"
  nil)

(def ^:dynamic ^:private *read-log*
  "Track cell dependencies during eval"
  nil)

(def ^:dynamic *tx-log*
  "Track cell changes during tx"
  nil)

(defn- mutate-cell! [cell new-attrs-js]
  (if *tx-log*
    (vswap! *tx-log* update cell #(doto (or % #js{})
                                    (gobj/extend new-attrs-js)))
    (-> (.-state cell)
        (gobj/extend new-attrs-js))))

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
      (let [newval (j/get changes .-value)
            oldval (j/get cell .-value)]
        (when (not= oldval newval)
          (notify-cell-changed! cell oldval newval))))
    value))

(defn internal-state [cell]
  (c/read cell .-internal))

(defn log-read! [cell]
  (when *read-log*
    (vswap! *read-log* conj cell))
  (r/log-read! cell)
  cell)


;;;;;;;;;;;;;;;;;;
;;
;; Cell status

(declare eval-dependents!)

(defn update-internal! [cell f & args]
  (c/update! cell .-internal #(apply f % args))

  ;; do not propagate?
  #_(tx! #(eval-dependents! cell))

  (r/invalidate-readers! cell))

(defn loading! [cell]
  (c/assoc! cell .-async #js [true nil]))

(defn error! [cell error]
  (c/assoc! cell .-async #js[false error]))

(defn complete! [cell]
  (c/assoc! cell .-async #js[false nil]))

;;;;;;;;;;;;;;;;;;
;;
;; Dependency graph

(def set-conj (fnil conj #{}))

(defn immediate-dependencies [cell]
  (c/read cell .-dependencies))

(defn immediate-dependents [cell]
  (c/read cell .-dependents))

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
  (c/assoc! cell .-dependencies
            (set-conj (immediate-dependencies cell) dep))
  (c/assoc! dep .-dependents
            (set-conj (immediate-dependents dep) cell)))

(defn remove-edge! [cell dep]
  (c/assoc! cell .-dependencies
            (disj (immediate-dependencies cell) dep))
  (c/assoc! dep .-dependents
            (disj (immediate-dependents dep) cell)))

(defn transition-deps! [cell next-dependency-nodes]
  (let [prev-dependencies (c/read cell .-dependencies)]
    (doseq [added (set/difference next-dependency-nodes prev-dependencies)]
      (depend! cell added))
    (doseq [removed (set/difference prev-dependencies next-dependency-nodes)]
      (remove-edge! cell removed))
    nil))

;;;;;;;;;;;;;;;;;;
;;
;; Cell evaluation

(defn handle-promise! [cell promise]
  ;; how to 'dispose' of a promise?
  (when-let [old-promise (c/read cell .-promise)]

    (c/assoc! cell .-promise promise))
  (let [done? (volatile! false)
        wrap-cb (fn [f]
                  (if (identical? promise (c/read cell .-promise))
                    (f)))]
    (-> promise
        (j/call :then (wrap-cb #(-reset! cell %)))
        (j/call :catch (wrap-cb #(error! cell %))))
    (when-not @done?
      (loading! cell)
      (-reset! cell nil))))

(defn- eval [cell]
  (let [f (c/read cell .-f)
        value (c/read cell .-value)]
    (try (f value)
         (catch js/Error e
           (runtime/dispose! tx-cell)
           (throw e)))))

(defn- eval-and-set! [cell]
  (if (= cell *cell*)
    cell
    (binding [*cell* cell
              *read-log* (volatile! #{})
              runtime/*runtime* (c/read cell .-runtime)]
      (runtime/dispose! cell)
      (let [value (eval cell)
            next-deps (disj @*read-log* cell)]
        (transition-deps! cell next-deps)
        (if (u/promise? value)
          (handle-promise! cell value)
          (-reset! cell value))))))

(def ^:dynamic ^:private *computing-dependents* false)

(defn- eval-dependents! [cell]
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
;; Cell type

(deftype Cell [state meta]

  Object
  (equiv [this other]
    (-equiv this other))

  IEquiv
  (-equiv [this other]
    (-equiv [this other]
            (if (instance? Cell other)
              (identical? state (.-state other))
              false)))

  IWithMeta
  (-with-meta [this m]
    (if (identical? m meta)
      this
      (Cell. state m)))

  IMeta
  (-meta [_] meta)

  IPrintWithWriter
  (-pr-writer [this writer _] (write-all writer (str "⚪️")))

  runtime/IDispose
  (on-dispose [this f]
    (c/update! this .-internal update ::on-dispose conj f))
  (-dispose! [this]
    (doseq [f (::on-dispose (internal-state this))]
      (f))
    (c/update! this .-internal dissoc ::on-dispose)
    this)

  ;; Atom behaviour

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [f (vals (::watches (internal-state this)))]
      (f this oldval newval)))
  (-add-watch [this key f]
    (c/update! this .-internal update ::watches assoc key f))
  (-remove-watch [this key]
    (c/update! this .-internal update ::watches dissoc key))

  IDeref
  (-deref [this]
    (log-read! this)
    (c/read this .-value))

  IReset
  (-reset! [this newval]
    (tx!
     (fn []
       (complete! this)
       (mutate-cell! this (j/obj .-value newval))
       (eval-dependents! this)))
    newval)

  ISwap
  (-swap! [this f] (-reset! this (f (c/read this .-value))))
  (-swap! [this f a] (-reset! this (f (c/read this .-value) a)))
  (-swap! [this f a b] (-reset! this (f (c/read this .-value) a b)))
  (-swap! [this f a b xs] (-reset! this (apply f (c/read this .-value) a b xs)))

  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (r/log-read! this)
    (case k
      :async/loading? (some-> (c/read this .-async) (aget 0))
      :async/error (some-> (c/read this .-async) (aget 1))
      (throw (js/Error. (str "Unknown key on cell: " k))))))

;;;;;;;;;;;;;;;;;;
;;
;; Cell construction

(defn- make-cell [f]
  (let [cell (Cell. (j/obj .-f f
                           .-value nil
                           .-dependencies #{}
                           .-dependents #{}
                           .-runtime runtime/*runtime*
                           .-internal {})
                    nil)]

    ;; this part clearly needs some work!
    (cond *cell* (runtime/on-dispose *cell* #(runtime/dispose! cell)) ;; dispose if 'owner' cell disposes?
          registry/*view* (v/on-unmount! registry/*view* cell #(runtime/dispose! cell)) ;; dispose if owned by view that unmounts?
          runtime/*runtime* (runtime/on-dispose runtime/*runtime* #(runtime/dispose! cell))) ;; dispose when 'runtime' disposes?
    ;; need to figure out - when do we want a cell to "stop" running? do we need a concept of a "runtime"?

    (tx! #(eval-and-set! cell))
    cell))

(defn cell*
  "Returns a new cell, or an existing cell if `id` has been seen before.
  `f` should be a function that, given the cell's previous value, returns its next value.
  `state` is not for public use."
  ([f]
   (cell* f nil))
  ([f {:keys [reload?
              prev-cell
              def?
              memo-key]}]
   (cond reload?
         (do (mutate-cell! prev-cell (j/obj .-f f
                                            .-value nil))
             (tx! #(eval-and-set! prev-cell))
             prev-cell)

         def? (make-cell f)

         memo-key (u/memoized-on (or *cell*
                                     registry/*view*

                                     ;; more thought to this.
                                     ;; do we need a runtime?
                                     runtime/*runtime*) memo-key
                    (make-cell f))
         :else (throw (js/Error. "Invalid arguments to `cell*`")))))


;; WIP
(defn cell
  [{:keys [key]} value]
  (assert key "Cells created by functions require a :key")
  (let [c (cell* (constantly value) {:memo-key (str "#" (hash key))})]
    (prn :key key c)
    c))