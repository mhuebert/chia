(ns hicada.interpret
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

(defn #?(:cljs ^string class-string
         :clj class-string) [classes]
  (cond (string? classes) classes
        (vector? classes) (str/join " " classes)
        :else classes))

#?(:cljs
   (defn update-class! [obj class-string]
         (j/update! obj :class
                    (fn [x]
                        (if (some? x)
                          (str class-string " " x)
                          class-string)))))

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

(defn form [x]
  (if (vector? x)
    (str "<hiccup: " x ">")
    x))
