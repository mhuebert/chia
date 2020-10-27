(ns hicada.interpreter
  (:require #?(:cljs [applied-science.js-interop :as j])
            [clojure.string :as str]
            [clojure.string :refer [blank? join]]
            [hicada.normalize :as normalize]
            [hicada.util :as util]
            hicada.macros))


(defprotocol IInterpreter
  (interpret [this] "Interpret a Clojure data structure as a React fn call."))

(defn create-element
  "Create a React element. Returns a JavaScript object when running
  under ClojureScript, and a om.dom.Element record in Clojure."
  [type props & children]
  #?(:clj  nil
     :cljs (apply js/React.createElement type props children)))

(defn classes-string [classes]
  (cond (string? classes) classes
        (vector? classes) (str/join " " classes)
        :else classes))

(defn- join-classes [m class-string classes]
  ;; joins `a` and `b` which may be nil
  (if (some? class-string)
    (assoc m :class (if (some? classes)
                      (str class-string " " (classes-string classes))
                      class-string))
    (if (some? classes-string)
      (assoc m :class classes-string)
      m)))


#?(:cljs
   (defn update-class! [obj class-string]
         (j/update! obj :class
                    (fn [x]
                        (if (some? x)
                          (str class-string " " x)
                          class-string)))))

(declare element)

(defn props
  [attrs]
  #?(:clj  (reduce-kv
             (fn [m k v] (util/compile-prop assoc m k v))
             {}
             attrs)
     :cljs (if (object? attrs)
             attrs
             (reduce-kv
               (fn [m k v] (util/interpret-prop j/assoc! m k v))
               #js{}
               attrs))))


(defn- interpret-seq
  "Eagerly interpret the seq `x` as HTML elements."
  [x]
  (into [] (map interpret) x))

(defn element
  "Render an element vector as a HTML element."
  [element]
  (let [[type attrs content] (normalize/hiccup-vec element)]
    (apply create-element type
           (props attrs)
           (interpret-seq content))))

(extend-protocol IInterpreter

  #?(:clj  clojure.lang.ChunkedCons
     :cljs cljs.core.ChunkedCons)
  (interpret [this]
    (interpret-seq this))

  ;;TODO: this type extension seems brittle.
  #?@(:cljs [cljs.core.Repeat
             (interpret [this]
                        (interpret-seq this))])

  #?(:clj  clojure.lang.PersistentVector$ChunkedSeq
     :cljs cljs.core.ChunkedSeq)
  (interpret [this]
    (interpret-seq this))

  #?(:clj  clojure.lang.Cons
     :cljs cljs.core.Cons)
  (interpret [this]
    (interpret-seq this))

  #?(:clj  clojure.lang.LazySeq
     :cljs cljs.core.LazySeq)
  (interpret [this]
    (interpret-seq this))

  #?(:clj  clojure.lang.PersistentList
     :cljs cljs.core.List)
  (interpret [this]
    (interpret-seq this))

  #?(:clj  clojure.lang.IndexedSeq
     :cljs cljs.core.IndexedSeq)
  (interpret [this]
    (interpret-seq this))

  #?(:clj  clojure.lang.APersistentVector$SubVector
     :cljs cljs.core.Subvec)
  (interpret [this]
    (element this))

  #?(:clj  clojure.lang.PersistentVector
     :cljs cljs.core.PersistentVector)
  (interpret [this]
    (element this))

  #?(:clj Object :cljs default)
  (interpret [this]
    this)

  nil
  (interpret [this]
    nil))
