(ns chia.graphql
  (:refer-clojure :exclude [munge defn fn let def])
  (:require [clojure.set :as set]
            [clojure.core :as core]
            [clojure.string :as string]
            [clojure.string :as str]))

(core/defn munge [s]
  (string/replace (if (keyword? s) (subs (str s) 1) s) #"[^a-zA-Z_0-9]+" "_"))

(core/defn namespace-segment []
  (string/replace (str *ns*) #"^[^.]+\." ""))

(core/defn op-name [the-name]
  (when the-name
    (munge (str (namespace-segment) "__" (name the-name)))))

(core/defn ensure-prefix [s pfx]
  (cond->> s
           (not (str/starts-with? s pfx)) (str pfx)))

(core/defn prefix-var-name [v]
  (-> v
      (name)
      (ensure-prefix "$")))

(defmacro operation*
  ([operation-name the-name {:as options
                             :keys [fn-wrap?]} [argmap :as arglist] body]
   (assert (or (empty? arglist)
               (and (= 1 (count arglist))
                    (map? (first arglist))))
           (str "Args must contain a map which destructures the desired keys"
                {:kind operation-name
                 :name the-name
                 :arglist arglist
                 :body body}))
   (assert (every? #(or (keyword? %) (symbol? %)) (keys argmap))
           "Only one level of destructuring is allowed in graphql function variables")

   (core/let [variable-map (merge
                            (->> (:keys argmap)
                                 (reduce (core/fn [m sym]
                                           (assoc m (prefix-var-name sym)
                                                    (:tag (meta sym)))) {}))
                            (->> (dissoc argmap :keys :or :as)
                                 (reduce-kv (core/fn [m sym k]
                                              (assoc m (prefix-var-name k)
                                                       (:tag (meta sym)))) {})))]
     `(core/let ~(if argmap
                   `[~(dissoc argmap :or) (reify
                                            ~'ILookup
                                            (~'-lookup [_# k#]
                                              (-> (name k#)
                                                  (~'chia.graphql/ensure-prefix "$")
                                                  (keyword)))
                                            (~'-lookup [o# k# not-found#]
                                              (throw (ex-info "lookup with not-found not supported" {:k k#})))
                                            ~'IDeref
                                            (~'-deref [_#]
                                              ~(reduce (core/fn [m k]
                                                         (assoc m (-> (name k)
                                                                      (str/replace-first "$" "")
                                                                      (keyword))
                                                                  (keyword k))) {}
                                                       (keys variable-map))))]
                   [])
        (~'chia.graphql/emit ~operation-name
         ~(cond->> `[~(op-name the-name)
                     (merge ~(dissoc options :fn-wrap?)
                            {:gql/operation ~operation-name}
                            (quote ~(-> variable-map
                                        (set/rename-keys {:or :gql/defaults}))))
                     ~@body]
                   fn-wrap? (list `core/fn [])))))))

(core/defn- var-operation [the-name]
  (core/let [m (meta the-name)]
    (cond (:Query m)
          "query"
          (:Mutation m)
          "mutation"
          (:Fragment m)
          "fragment"
          :else (do (prn :unknown-sym-operation m) nil))))

(core/defn parse-args [args arglist?]
  (core/let [[var-name args] (if (symbol? (first args))
                               [(first args) (rest args)]
                               [nil args])
             [docstring args] (if (string? (first args))
                                [(first args) (rest args)]
                                [nil args])
             [opts args] (if (map? (first args))
                           [(first args) (rest args)]
                           [nil args])
             [arglist args] (if (and arglist?
                                     (vector? (first args))
                                     (seq (rest args)))
                              [(first args) (rest args)]
                              [nil args])]
    [var-name opts arglist args]))

(defmacro fragment [& args]
  (core/let [[the-name opts arglist body] (parse-args args false)]
    `(~'chia.graphql/operation*
      "fragment"
      ~(or the-name (str (gensym)))
      ~opts
      ~arglist
      ~body)))

(defmacro def [& args]
  (core/let [operation (var-operation (first args))
             [the-name opts arglist body] (parse-args args false)]
    `(def ~the-name
       (~'chia.graphql/operation*
        ~operation
        ~the-name
        ~opts
        ~arglist
        ~body))))

(defmacro fn [& args]
  (core/let [operation (var-operation (first args))
             [the-name opts arglist body] (parse-args args true)]
    (assert arglist "GraphQL function must contain arglist")
    `(~'chia.graphql/operation*
      ~operation
      ~the-name
      ~(assoc opts :fn-wrap? true)
      ~arglist
      ~body)))

(defmacro defn [the-name & args]
  `(def ~the-name
     (~'chia.graphql/fn ~@(cons the-name args))))
