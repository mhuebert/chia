(ns yawn.env
  (:require [clojure.walk :as walk]
            #?@(:clj  [[net.cgrand.macrovich :as macros]]
                :cljs [[applied-science.js-interop :as j]
                       yawn.react]))
  #?(:cljs (:require-macros yawn.env
                            [net.cgrand.macrovich :as macros])))

(defn merge-opts [x y]
  (merge-with (fn [x y] (if (map? x) (merge x y) y)) x y))

(defonce !compile-opts (atom {}))

(defn get-opts [sym] (or (@!compile-opts sym)
                         (prn :not-found sym (keys @!compile-opts))
                         (throw (ex-info "Compile opts not found" {:sym sym
                                                                   :keys (keys @!compile-opts)}))))

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
        stage (fn [form pick]
                (walk/postwalk (fn [x]
                                 {:pre [(#{:compile :interpret} pick)]}
                                 (if (and (map? x) (:compile x))
                                   (get x pick)
                                   x)) form))
        js-form `(~'applied-science.js-interop/lit ~(-> (dequote opts)
                                                        (stage :interpret)
                                                        (dissoc :warn-on-interpretation?
                                                                :skip-types
                                                                :rewrite-for?
                                                                :create-element-compile)))
        qualified-name `(quote ~(qualified-sym name))
        clj-form (-> opts
                     (stage :compile)
                     (assoc :js-options-sym qualified-name)
                     (update :skip-types quote-it)
                     (update :custom-elements quote-it)
                     (update :create-element quote-it)
                     (update :create-element-compile quote-it))]

    `(do
       (swap! !compile-opts assoc ~qualified-name ~clj-form)
       ~(macros/case :cljs `(def ~name ~js-form)
                     :clj `(def ~name ~clj-form)))))
