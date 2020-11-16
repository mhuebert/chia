(ns yawn.env
  (:require #?@(:clj  [[net.cgrand.macrovich :as macros]]
                :cljs [[applied-science.js-interop :as j]
                       yawn.react]))
  #?(:cljs (:require-macros yawn.env
                            [net.cgrand.macrovich :as macros])))

(defn merge-opts [x y]
  (merge-with (fn [x y] (if (map? x) (merge x y) y)) x y))

(macros/deftime

  (defonce defaults (atom nil))
  (defn set-defaults! [options] (reset! defaults options))

  (defn dequote [x]
    (if (list? x) (second x) x))

  (defn qualified-sym [n]
    (symbol (name (.-name *ns*)) (name n)))

  (defmacro def-options
    [name opts]
    (assert @defaults "Defaults have not yet been set")
    (let [opts (merge-opts @defaults opts)
          quote-it (fn [x] `(quote ~x))
          js-form `(~'applied-science.js-interop/lit ~(dissoc (dequote opts)
                                                              :warn-on-interpretation?
                                                              :skip-types
                                                              :rewrite-for?
                                                              :create-element-compile))
          clj-form (-> opts
                       (assoc :js-options-sym `(quote ~(qualified-sym name)))
                       (update :skip-types quote-it)
                       (update :custom-elements quote-it)
                       (update :create-element quote-it)
                       (update :create-element-compile quote-it))]

      (macros/case :cljs `(def ~name ~js-form)
                   :clj `(def ~name ~clj-form)))))
