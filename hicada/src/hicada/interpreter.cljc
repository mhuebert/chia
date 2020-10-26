(ns hicada.interpreter
  (:require #?(:cljs [goog.object :as object])
            [clojure.string :as str]
            [clojure.string :refer [blank? join]]
            [hicada.normalize :as normalize]
            [hicada.util :as util]))

(defprotocol IInterpreter
  (interpret [this] "Interpret a Clojure data structure as a React fn call."))

(defn create-element
  "Create a React element. Returns a JavaScript object when running
  under ClojureScript, and a om.dom.Element record in Clojure."
  [type props & children]
  #?(:clj nil
     :cljs (apply js/React.createElement type props children)))

(defn attributes [attrs]
  #?(:clj (-> (util/html-to-dom-attrs attrs)
              (update :className #(some->> % (str/join " "))))
     :cljs (when-let [js-attrs (clj->js (util/html-to-dom-attrs attrs))]
             (let [class (.-className js-attrs)
                   class (if (array? class) (join " " class) class)]
               (if (blank? class)
                 (js-delete js-attrs "className")
                 (set! (.-className js-attrs) class))
               js-attrs))))

(defn- interpret-seq
  "Eagerly interpret the seq `x` as HTML elements."
  [x]
  (into [] (map interpret) x))

(defn element
  "Render an element vector as a HTML element."
  [element]
  (let [[type attrs content] (normalize/element element)]
    (apply create-element (name type) ;;hicada uses keyword tags, unlike sablono
           (attributes attrs)
           (interpret-seq content))))

(defn- interpret-vec
  "Interpret the vector `x` as an HTML element or a the children of an
  element."
  [x]
  (if (util/hiccup-vector? x)
    (element x)
    (interpret-seq x)))

(extend-protocol IInterpreter

  #?(:clj clojure.lang.ChunkedCons
     :cljs cljs.core.ChunkedCons)
  (interpret [this]
    (interpret-seq this))

  ;;TODO: this type extension seems brittle.
  #?@(:cljs [cljs.core.Repeat
             (interpret [this]
                        (interpret-seq this))])

  #?(:clj clojure.lang.PersistentVector$ChunkedSeq
     :cljs cljs.core.ChunkedSeq)
  (interpret [this]
    (interpret-seq this))

  #?(:clj clojure.lang.Cons
     :cljs cljs.core.Cons)
  (interpret [this]
    (interpret-seq this))

  #?(:clj clojure.lang.LazySeq
     :cljs cljs.core.LazySeq)
  (interpret [this]
    (interpret-seq this))

  #?(:clj clojure.lang.PersistentList
     :cljs cljs.core.List)
  (interpret [this]
    (interpret-seq this))

  #?(:clj clojure.lang.IndexedSeq
     :cljs cljs.core.IndexedSeq)
  (interpret [this]
    (interpret-seq this))

  #?(:clj clojure.lang.APersistentVector$SubVector
     :cljs cljs.core.Subvec)
  (interpret [this]
    (interpret-vec this))

  #?(:clj clojure.lang.PersistentVector
     :cljs cljs.core.PersistentVector)
  (interpret [this]
    (interpret-vec this))

  #?(:clj Object :cljs default)
  (interpret [this]
    this)

  nil
  (interpret [this]
    nil))
