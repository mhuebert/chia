(ns chia.view.util
  (:refer-clojure :exclude [uuid])
  (:require [applied-science.js-interop :as j]))

(defn update-props [el f & args]
  (if-not (vector? el)
    el
    (let [attrs? (map? (second el))]
      (into [(el 0) (apply f (if attrs? (el 1) {}) args)]
            (subvec el (if attrs? 2 1))))))

(defn ensure-keys [forms]
  (let [seen #{}]
    (map-indexed #(update-props %2 update :key (fn [k]
                                                 (if (or (nil? k) (contains? seen k))
                                                   %1
                                                   (do (swap! seen conj k)
                                                       k)))) forms)))

(defn map-with-keys [& args]
  (ensure-keys (apply clojure.core/map args)))

(defn any-pred
  "Evaluate fns sequentially, stopping if any return true."
  [& fns]
  (fn [this]
    (loop [fns fns]
      (if (empty? fns)
        false
        (or ((first fns) this)
            (recur (rest fns)))))))

(defn flatten-seqs
  "Flatten collection, only unwrap sequences"
  [children]
  (filter #(not (seq? %))
          (rest (tree-seq seq? seq children))))

#?(:cljs
   (defn find-or-append-element
     ([id] (find-or-append-element id :div))
     ([id tag]
      (or (.getElementById js/document id)
          (let [element (-> (.createElement js/document (name tag))
                            (j/assoc! :id (name id)))]
            (.. js/document -body (appendChild element)))))))

(def lifecycle-keys
  "Mapping of methods-map keys to React lifecycle keys."
  {:view/initial-state "chia$initialState"
   :view/did-catch "componentDidCatch"
   :view/did-mount "componentDidMount"
   :static/get-derived-state-from-props "getDerivedStateFromProps"
   :view/will-receive-state "componentWillReceiveState"
   :view/should-update "shouldComponentUpdate"
   :view/did-update "componentDidUpdate"
   :view/will-unmount "componentWillUnmount"
   :view/render "render"})

(defn _state-key? [k]
  (case k
    (:view/props
     :view/prev-props
     :view/state
     :view/prev-state
     :view/children
     :view/prev-children) true false))