(ns chia.view.util
  (:refer-clojure :exclude [uuid])
  (:require [applied-science.js-interop :as j]))

(defn update-props [el f & args]
  (assert (vector? el))
  (let [attrs? (map? (second el))]
    (into [(el 0) (apply f (if attrs? (el 1) {}) args)]
          (subvec el (if attrs? 2 1)))))

(defn flatten-seqs
  "Flatten collection, only unwrap sequences"
  [children]
  (filter #(not (seq? %))
          (rest (tree-seq seq? seq children))))

(defn parse-args [args]
  (if (or (map? (first args))
          (nil? (first args)))
    [(first args) (rest args)]
    [{} args]))

#?(:cljs
   (defn find-or-append-element
     ([id] (find-or-append-element id :div))
     ([id tag]
      (or (.getElementById js/document id)
          (let [element (-> (.createElement js/document (name tag))
                            (j/assoc! :id (name id)))]
            (.. js/document -body (appendChild element)))))))