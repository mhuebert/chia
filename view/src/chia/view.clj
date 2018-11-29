(ns chia.view
  (:refer-clojure :exclude [for])
  (:require [clojure.string :as str]
            [chia.util :as u]
            [chia.view.util :as view-util]
            [clojure.core :as core]
   #_[cljs.tagged-literals :as cljs-literals]))

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

(defn to-element [x]
  ;; TODO
  ;; upgrade hiccup/element to work partly at macroexpansion time
  `(~'chia.view.hiccup/element {:wrap-props ~'chia.view/wrap-props}
    ~x))

(defn wrap-current-view-binding [body]
  `(~'this-as this#
    (binding [*current-view* this#]
      ~body)))

(defn- wrap-render-body
  "Wrap body in anonymous function form."
  [name [argv & body] pure?]
  (assert (vector? argv)
          (str "View " name " is missing an argument vector"))
  `(~'fn ~(symbol (str "__" name)) ~argv
    ~(cond-> (to-element `(do ~@body))
             (not pure?) (wrap-current-view-binding))))



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
                                 (into out [(u/camel-case (name k)) v])) []))))

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
                                        (contains? view-util/lifecycle-keys k) :lifecycle-keys
                                        (contains? view-util/__deprecated-keys k) (throw (ex-info "Deprecated React lifecycle key" {:key k}))
                                        (nil? the-ns) :unqualified-keys
                                        :else :qualified-keys)) k] v)) {} methods)
      (update :unqualified-keys (comp ->js-with-camelCase bind-vals))))


(defmacro view
  [& args]
  (let [[view-name docstring methods body] (view-util/parse-opt-args [symbol? string? map?] args)
        display-name (get-display-name *ns* view-name)
        {pure? :pure} (meta view-name)
        {:as methods
         :keys [lifecycle-keys]} (-> methods
                                     (merge (cond-> {;; TODO
                                                     ;; keep track of dev- vs prod-time, elide display-name and docstring in prod
                                                     :display-name display-name
                                                     :view/render (wrap-render-body view-name body pure?)}
                                                    docstring (assoc :docstring docstring)))
                                     (group-methods))]

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
  (let [[view-name docstring _] (view-util/parse-opt-args [symbol? string?] args)
        _ (assert (symbol? view-name))]

    `(def ~view-name ~@(some-> docstring (list))
       (~'chia.view/view ~@args))))

(defmacro extend-view [view & args]
  `(clojure.core/specify!
    (~'goog.object/getValueByKeys ~view "chia$constructor" "prototype")
    ~@args))

(defmacro once
  "Evaluates `body` once per component mount or, if :key is provided, once per unique key (per component mount).

  :on-unmount - will be called with [component, value] when component unmounts."
  ([body]
   `(once {} ~body))
  ([{:keys [key
            on-unmount]} body]
   (let [gname (name (gensym "once"))
         js-get 'chia.util.js-interop/get
         js-assoc! 'chia.util.js-interop/assoc!
         this-sym (gensym "this")
         key-sym (gensym "key")
         val-sym (gensym "val")]
     `(let [~key-sym ~(if key `(str ~gname "/" ~key)
                              gname)
            ~this-sym ~'chia.view/*current-view*]
        (or (~js-get ~this-sym ~key-sym)
            (let [~val-sym ~body]
              (~js-assoc! ~this-sym ~key-sym ~val-sym)
              ~(when on-unmount
                 `(~'chia.view/on-unmount! ~this-sym ~key-sym
                   (fn [this#] (~on-unmount this# ~val-sym))))
              ~val-sym))))))

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
