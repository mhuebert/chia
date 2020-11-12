(ns hicada.env
  (:require [clojure.walk :as walk]
            #?(:clj [net.cgrand.macrovich :as macros]
               :cljs [applied-science.js-interop :as j]))
  #?(:cljs (:require-macros hicada.env
                            [net.cgrand.macrovich :as macros])))

(defn merge-opts [x y]
  (merge-with (fn [x y] (if (map? x) (merge x y) y)) x y))

(macros/deftime

  (def defaults '{;; settings for the compiler:
                  :warn-on-interpretation? true
                  :skip-types '#{number
                                 string
                                 function
                                 js}
                  :rewrite-for? true

                  ;; relevant for the interpreter:
                  :create-element-tag ">"
                  :custom-elements {"Fragment" hicada.react/Fragment
                                    "<>" hicada.react/Fragment
                                    "Suspense" hicada.react/Suspense}
                  :create-element hicada.react/createElement
                  :convert-form hicada.convert/as-element
                  :convert-props hicada.convert/convert-props
                  :convert-class hicada.convert/class-string
                  :update-class hicada.convert/update-class!
                  :assoc-prop applied-science.js-interop/!set})

  (defn dequote [x]
    (if (list? x) (second x) x))

  (defmacro parse-opts
    [opts]
    (let [opts (merge-opts defaults opts)]
      (macros/case :clj (walk/postwalk (fn [x] (if (symbol? x)
                                                 `(quote ~x)
                                                 x)) opts)
                   :cljs `(delay
                            (j/lit ~(dissoc (dequote opts)
                                            :warn-on-interpretation?
                                            :skip-types
                                            :rewrite-for?)))))))

(def default-options (atom {}))

(defn with-defaults [options]
  (merge-opts @default-options options))

(def ^:dynamic *options* nil)
