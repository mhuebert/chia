(ns hicada.interpreter
  (:require #?@(:cljs
                [[applied-science.js-interop :as j]
                 ["react" :as react]
                 ["react-dom" :as react-dom]])
            [clojure.string :as str]
            [clojure.string :refer [blank? join]]
            [hicada.normalize :as normalize]
            [hicada.util :as util]
            [hicada.macros]))

#?(:cljs
   (do
     (def Fragment react/Fragment)
     (def createElement react/createElement)))

(defn create-element
  "Create a React element. Returns a JavaScript object when running
  under ClojureScript, and a om.dom.Element record in Clojure."
  [type props & children]
  #?(:clj  nil
     :cljs (apply createElement type props children)))

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

(defn interpret [x]
  (prn :interpreting x)
  (if (vector? x)
    (str "<hiccup: " x ">")
    x))
