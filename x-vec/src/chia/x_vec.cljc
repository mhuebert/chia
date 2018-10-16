(ns chia.x-vec
  (:require [chia.util :as u]))

(declare -emit)

(def ^:dynamic *parse-tag* identity)
(def ^:dynamic *emit-list* identity)
(def ^:dynamic *emit-nonvec* identity)

(defn ^:dynamic *emit-vec*
  "default implementation of `*emit-vec*` simply"
  [tag props children]
  (into [tag props] children))

(def ^:dynamic *depth* 1)

(defprotocol IElement
  (to-element [this] "Returns an element representing `this`"))

(defprotocol IEmitXvec
  (to-hiccup [this] "Returns an x-vec representing `this`"))

(defn get-props [form]
  (some-> (second form)
          (u/guard map?)))

(defn parse-vec
  "Return props and children for a hiccup form. If the second element is not a map, supplies an empty map as props."
  [form]
  (let [len (count form)]
    (cond (= len 1) [{} []]
          (let [first-child (form 1)]
            (or (nil? first-child)
                (instance? PersistentArrayMap first-child)
                (instance? PersistentHashMap first-child))) [(form 1) (if (> len 2) (subvec form 2 len) [])]
          :else [{} (subvec form 1 len)])))

(defn flatten-seqs
  "Recursively apply f to nested vectors, unwrapping seqs. Similar to recursive `mapcat` but returns a vector."
  [f init coll]
  (reduce (fn my-f [c x]
            (if (seq? x)
              (flatten-seqs f c x)
              (conj c (f x)))) init coll))

(defn -emit [form]
  (cond (vector? form)
        (try
          (let [[props children] (parse-vec form)
                tag (form 0)
                function-form? (fn? tag)
                [tag props] (cond-> [tag props]
                                    (not function-form?) (*parse-tag*))
                children (binding [*depth* (inc *depth*)]
                           (flatten-seqs -emit [] children))]

            (*emit-vec* tag props children))

          (catch js/Error e
            (println "Error in render-hiccup-node:")
            (println form)
            (.error js/console e)))


        (satisfies? IElement form)
        (to-element form)

        (satisfies? IEmitXvec form)
        (-emit (to-hiccup form))

        (list? form)
        (*emit-list* (map -emit form))

        :else (*emit-nonvec* form)))

(defn emit
  "Returns element for provided x-vec form, based on current dynamic bindings.
  If a non-vector form is supplied, it is returned untouched."
  ([form]
   (-emit form))
  ([{:keys [emit-vec parse-tag emit-list emit-nonvec]} form]
   (binding [*parse-tag* (or parse-tag *parse-tag*)

             *emit-vec* (or emit-vec *emit-vec*)
             *emit-list* (or emit-list *emit-list*)
             *emit-nonvec* (or emit-nonvec *emit-nonvec*)]
     (-emit form))))