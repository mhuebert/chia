(ns chia.view
  (:refer-clojure :exclude [for])
  (:require [clojure.string :as str]
            [chia.view.util :as u :refer [camelCase]]
            [clojure.core :as core]
            [cljs.tagged-literals :as cljs-literals]))

(defmacro ^:private apply-fn [f this]
  `(if-let [children# (.. ~this -state -children)]
     (.apply ~f ~this (to-array (cons ~this children#)))
     (.call ~f ~this ~this)))

(defn- get-display-name
  "Generate a meaningful name to identify React components while debugging"
  [ns given-name]
  (let [segments (->> (str/split (name (ns-name ns)) #"\.")
                      (drop 1)
                      (take-last 2))]
    (str (str/join "." segments)
         "/"
         (or given-name
             (gensym "view")))))

(defn wrap-element [x]
  ;; TODO
  ;; upgrade hiccup/element to work partly at macroexpansion time
  `(~'chia.view.hiccup/element ~x
    {:wrap-props ~'chia.view/wrap-props}))

(defn wrap-class [x css-name]
  (assert (vector? x)
          (str ":view/css requires a literal vector to be returned from the render method. In: " css-name))
  (if (map? (second x))
    (update-in x [1 :class] str " " css-name)
    `(~'chia.view.hiccup/update-attr ~x str ~(str " " css-name))))

(defn- wrap-render-body
  "Wrap body in anonymous function form."
  [name [args & body] css-name]
  (assert (vector? args))
  (let [effects (drop-last body)
        return-element (cond-> (last body)
                               css-name (wrap-class css-name))]
    `(~'fn ~(symbol (str name \*)) ~args
      ~@effects
      ~(wrap-element return-element))))

(defn- make-constructor [the-name]
  (let [this-name (gensym)
        fn-name (gensym the-name)]
    `(fn ~fn-name [$props#]
       (core/this-as ~this-name
                     ;; super()
                     (~'.call ~'chia.view/Component ~this-name $props#)
                     ;; init internal state
                     (set! (~'.-state ~this-name) (~'js-obj))
                     ;; add count
                     (-> ~this-name
                         (~'goog.object/set "chia$order" (~'vswap! ~'chia.view/instance-counter inc)))

                     ;; init state atom
                     (~'chia.view/init-state-atom! ~this-name $props#)

                     ;; return component
                     ~this-name))))

(defn- ->js-with-camelCase [m]
  `(~'js-obj ~@(->> m
                    (reduce-kv (fn [out k v]
                                 (into out [(camelCase (name k)) v])) []))))

(defn- bind-vals [m]
  (reduce-kv (fn [m k v]
               (assoc m k `(let [v# ~v]
                             (if (fn? v#)
                               (fn [& args#]
                                 (~'this-as this#
                                  (apply v# this# args#)))
                               v#)))) {} m))

(defn- group-methods
  "Groups methods by role in a React component."
  [methods]
  (-> (reduce-kv (fn [m k v]
                   (assoc-in m [(let [the-ns (namespace k)]
                                  (cond (= the-ns "static") :static-keys
                                        (contains? u/lifecycle-keys k) :lifecycle-keys
                                        (nil? the-ns) :unqualified-keys
                                        :else :qualified-keys)) k] v)) {} methods)
      (update :unqualified-keys (comp ->js-with-camelCase bind-vals))))


(defmacro view
  [& args]
  (let [[view-name docstring methods body] (u/parse-opt-args [symbol? string? map?] args)
        display-name (get-display-name *ns* view-name)
        css? (:view/css methods)
        css-name (when css? (str "chia__" (gensym view-name)))
        {:as methods
         :keys [lifecycle-keys]} (-> methods
                                     (merge (cond-> {;; TODO
                                                     ;; keep track of dev- vs prod-time, elide display-name and docstring in prod
                                                     :display-name display-name
                                                     :view/render (wrap-render-body view-name body (when css?
                                                                                                     css-name))}
                                                    docstring (assoc :docstring docstring)
                                                    css? (assoc :view/css-name css-name)))
                                     (group-methods))
        {pure? :pure} (meta view-name)]

    (when (and pure? (seq (dissoc lifecycle-keys :view/render)))
      (throw (ex-info "Warning: lifecycle methods are not supported on pure components." {:name view-name
                                                                                          :methods methods})))

    (if pure? (:view/render lifecycle-keys)
              (let [constructor (make-constructor view-name)]
                `(~'chia.view/view* ~methods ~constructor)))))
(defmacro defview
  "Define a view function.

   Expects optional docstring and methods map, followed by
    the argslist and body for the render function, which should
    return a Hiccup vector or React element."
  [& args]
  (let [[view-name docstring _] (u/parse-opt-args [symbol? string?] args)
        _ (assert (symbol? view-name))]

    `(def ~view-name ~@(some-> docstring (list))
       (~'chia.view/view ~@args))))

(defmacro extend-view [view & args]
  `(clojure.core/specify!
    (~'goog.object/getValueByKeys ~view "chia$constructor" "prototype")
    ~@args))

(comment
 (assert (= (parse-view-args '(name "a" {:b 1} [c] 1 2))
            '[name "a" {:b 1} ([c] 1 2)]))

 (assert (= (parse-view-args '(name {} [] 1 2))
            '[name nil {} ([] 1 2)]))

 (assert (= (parse-view-args '(name "a" [] 1 2))
            '[name "a" nil ([] 1 2)]))

 (assert (= (parse-view-args '(name [] 1 2))
            '[name nil nil ([] 1 2)]))

 (assert (= (parse-view-args '(name []))
            '[name nil nil ([])])))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpful macros
;; - inspired by https://gist.github.com/rauhs/d49d6f8a6f5fbb8230647c5b2ac210b2

(defmacro for
  "Simplified `for`, acts on a single collection; returns array and wraps with hiccup."
  [[x coll] body]
  `(reduce (fn [a# ~x]
             (.push a# ~(wrap-element body))
             a#)
           (cljs.core/array) ~coll))

(defmacro for-indexed
  "Simplified `for` with index, acts on a single collection; returns array and wraps with hiccup.

   (for-indexed [[index item] coll] body)"
  [[[idx item] coll] body]
  `(let [coll# ~coll
         a# (cljs.core/array)]
     (reduce (fn [~idx ~item]
               (.push a# ~(wrap-element body))
               (inc ~idx))
             0 coll#)
     a#))

#_(def js-value-type (type (cljs-literals/->JSValue [])))