(ns chia.view.props
  (:refer-clojure :exclude [partial])
  (:require [chia.util :as u]
            [chia.view.hiccup.impl :as hiccup-impl]))

(defn- update-prop-keys [props updates]
  (->> updates
       (reduce-kv
        (fn [m update-f ks]
          (->> ks
               (reduce (fn [m k]
                         (cond-> m
                                 (contains? m k) (update k update-f))) m))) props)))

(defn adapt-props
  "Converts props map to JavaScript according to `options`.
  lift-nses:      coll of namespaces (as strings), keys of these namespaces
                    will be included (all other namespaced keys are elided)
  wrap-props:     arbitrary fn to modify props map after other transformations"
  [{:keys [updates
           lift-nses
           defaults]
    :as   opts} props]
  (-> (merge defaults props)
      (cond-> lift-nses (u/lift-nses lift-nses)
              updates (update-prop-keys updates))
      (hiccup-impl/update-change-prop)
      (hiccup-impl/props->js)))

(defn merge-props
  "Merge props, concatenating :class props and merging styles."
  [m1 m2]
  (merge m1
         m2
         (merge-with #(str (hiccup-impl/class-str %1) " " (hiccup-impl/class-str %2))
                     (select-keys m1 [:class])
                     (select-keys m2 [:class]))
         (merge-with merge
                     (select-keys m1 [:style])
                     (select-keys m2 [:style]))))

(defn partial
  "Partially applies props to view. Keys will be merged with other props."
  [view initial-props]
  (fn [props & children]
    (let [[props children] (if (or (map? props)
                                   (nil? props)) [props children]
                                                 [{} (cons props children)])]
      (into [view (merge-props initial-props props)] children))))
