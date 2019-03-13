(ns chia.cell.deps
  (:require [com.stuartsierra.dependency :as dep]
            [clojure.set :as set]))

(defn id [cell] (.-id cell))

(defn- transitive
  "Recursively expands the set of dependency relationships starting
  at (get neighbors x), for each x in node-set"
  [k cell]
  (loop [unexpanded (mapcat (comp k cells id) node-set)
         expanded #{}]
    (if-let [[node & more] (seq unexpanded)]
      (if (contains? expanded node)
        (recur more expanded)
        (recur (concat more (get-in cells [(id node) k]))
               (conj expanded node)))
      expanded)))

(defn immediate-dependencies [cell]
  (.-dependencies cell))

(defn immediate-dependents [cell]
  (.-dependents cell))

(defn transitive-dependencies [node]
  (transitive :dependencies node))

(defn transitive-dependents [node]
  (transitive :dependents node))

(defn nodes [cell]
  (into (transitive-dependencies cell)
        (transitive-dependents cell)))

(defrecord CellGraph [cell]

  dep/DependencyGraph
  (immediate-dependencies [graph node]

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
    (CellGraph. (dissoc cells (id node)))))

(defn transition-deps [graph node next-dependency-nodes]
  (let [prev-dependencies (dep/immediate-dependencies graph node)
        graph (->> (set/difference next-dependency-nodes prev-dependencies)
                   (reduce (fn [graph added]
                             (dep/depend graph node added)) graph))
        graph (->> (set/difference prev-dependencies next-dependency-nodes)
                   (reduce (fn [graph removed]
                             (dep/remove-edge graph node removed)) graph))]
    graph))

(defn topo-sort [graph nodes]
  (let [comparator (dep/topo-comparator graph)]
    (sort comparator nodes)))

(defn transitive-dependents-sorted [graph node]
  (topo-sort graph (dep/transitive-dependents graph node)))