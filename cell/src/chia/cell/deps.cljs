(ns chia.cell.deps
  (:require [com.stuartsierra.dependency :as dep]
            [clojure.set :as set]))

(defrecord CellGraph [cells dependencies dependents]

  dep/DependencyGraph
  (immediate-dependencies [graph id]
    (get dependencies id #{}))
  (immediate-dependents [graph id]
    (get dependents id #{}))
  (transitive-dependencies [graph id]
    (dep/transitive dependencies #{id}))
  (transitive-dependencies-set [graph id-set]
    (dep/transitive dependencies id-set))
  (transitive-dependents [graph id]
    (dep/transitive dependents #{id}))
  (transitive-dependents-set [graph id-set]
    (dep/transitive dependents id-set))
  (nodes [graph]
    (set (keys cells)))

  dep/DependencyGraphUpdate
  (depend [graph id dep-id]
    (when (or (= id dep-id) (dep/depends? graph dep-id id))
      (throw (ex-info (str "Circular dependency between "
                           (pr-str id) " and " (pr-str dep-id))
                      {:reason ::circular-dependency
                       :node id
                       :dependency dep-id})))
    (assert (and id (keyword? id)) "missing node")
    (assert (and dep-id (keyword? dep-id)) "missing dep-id")

    (CellGraph. cells
                (update dependencies id dep/set-conj dep-id)
                (update dependents dep-id dep/set-conj id)))
  (remove-edge [graph id dep-id]
    (assert (and id (keyword? id)) "missing node")
    (assert (and dep-id (keyword? dep-id)) "missing dep-id")

    (CellGraph. cells
                (update dependencies id disj dep-id)
                (update dependents dep-id disj id)))
  (remove-all [graph id]
    (CellGraph. (dissoc cells id)
                (dep/remove-from-map dependencies id)
                (dep/remove-from-map dependents id)))
  (remove-node [graph id]
    (assert (and id (keyword? id)) "missing node")
    (CellGraph. (dissoc cells id)
                (dissoc dependencies id)
                dependents)))

(defn transition-deps [graph id next-dependency-ids]
  (assert (every? keyword? next-dependency-ids))
  (let [prev-dependencies (dep/immediate-dependencies graph id)
        graph (->> (set/difference next-dependency-ids prev-dependencies)
                   (reduce (fn [graph added]
                             (dep/depend graph id added)) graph))
        graph (->> (set/difference prev-dependencies next-dependency-ids)
                   (reduce (fn [graph removed]
                             (dep/remove-edge graph id removed)) graph))]
    graph))

(defn topo-sort [graph ids]
  (let [comparator (dep/topo-comparator graph)]
    (sort comparator ids)))

(defn transitive-dependents-sorted [graph id]
  (topo-sort graph (dep/transitive-dependents graph id)))