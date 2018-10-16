(ns chia.graphql
  (:require [chia.x-vec :as x]
            [clojure.set :as set]
            [clojure.string :as str])
  (:require-macros chia.graphql))

(def ^:dynamic *fragments* nil)

(def spacer "  ")

(defn depth-spacing
  ([] (depth-spacing 0))
  ([n]
   (apply str (take (+ x/*depth* n) (repeat spacer)))))

(defn ensure-prefix [s pfx]
  (cond->> s
           (not (str/starts-with? s pfx)) (str pfx)))

(defprotocol IGraphQL)

(defn emit-value [x]
  (cond (string? x) (str \" x \")
        (or (keyword? x) (symbol? x)) (let [[original-name base-name collection? required?] (re-matches #"([^+!]+)(\+)?(!)?" (name x))]
                                        (if collection?
                                          (str "[" base-name "]"
                                               (when required? "!"))
                                          original-name))
        (boolean? x) (str x)
        (number? x) x
        (nil? x) "null"
        (vector? x) (str "[" (str/join ", " (mapv emit-value x)) "]")
        (map? x)
        (let [props? (::props (meta x))
              defaults (:gql/defaults x)
              [lbracket rbracket] (if props? ["(" ")"] ["{" "}"])]
          (when-let [fields (seq (keep (fn [[key value]]
                                         (when-not (and (keyword? key) (= "gql" (namespace key)))
                                           (str (name key) ": " (emit-value value)
                                                (when-some [default (and props? (get defaults key))]
                                                  (str " = " (emit-value default)))))) x))]
            (str lbracket (str/join ", " fields) rbracket)))))

(defn emit-children [children]
  (when (seq children)
    (str " {"
         (str/join
          (let [spacing (depth-spacing)]
            (for [child children]
              (str \newline spacing child))))
         (str \newline (depth-spacing -1) "}"))))

(comment
 (assert (= (emit-value {:a 1 :b 2}) "{a: 1, b: 2}"))
 (assert (= (emit-value ^::props {:a 1 :b 2}) "(a: 1, b: 2)")))

(defn vec-wrap [x]
  (cond (keyword? x) [x {}]
        (and (vector? x) (not (map? (second x))))
        (into [(first x) {}] (rest x))
        :else x))

(comment
 (= (vec-wrap :x) [:x {}])
 (= (vec-wrap [:x]) [:x {}])
 (= (vec-wrap [:x :y]) [:x {} :y]))

(defn update-last [v f & args]
  (apply update v (dec (count v)) f args))

(defn emit-vec [tag {:as props
                     :keys [gql/operation
                            gql/directives
                            gql/alias-of
                            gql/defaults]} children]
  (let [{:as props
         :keys [gql/on]} (cond-> props
                                 (or (= "fragment" operation)
                                     (= tag :...))
                                 (set/rename-keys {:on :gql/on}))]
    (-> (cond-> []
                operation (conj (name operation))
                tag (conj (name tag))
                on (into ["on" (name on)])
                alias-of (-> (update-last str ":")
                             (conj (name alias-of))))
        (->> (str/join " "))
        (str (emit-value (with-meta props {::props true}))
             (apply str (for [[directive directive-props] directives]
                          (str " @" (name directive) (emit-value (with-meta directive-props {::props true})))))
             (emit-children children)))))

(defn emit-nonvec [x]
  (cond (or (nil? x)
            (string? x)) x
        (keyword? x) (name x)
        (number? x) (str x)
        (and (satisfies? IGraphQL x)
             (= "fragment" (:operation x)))
        (do (some-> *fragments* (swap! conj x))
            (str "..." (name x)))
        :else (do
                (prn :nonvec-emit-error x)
                (throw (js/Error. (str "Not a keyword or number: " x ", " (type x)))))))

(defmulti graphql-lookup (fn [o k not-found] k))

(defmethod graphql-lookup :default
  [this k not-found]
  (when goog.DEBUG (js/console.warn (str ::graphql-lookup
                                         "Tried to look up unknown key on GraphQL instance: " k)))
  not-found)

(deftype GraphQL [form
                  operation
                  string
                  string+                                   ;; includes transitive deps
                  fragments
                  fragments+]
  IGraphQL
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "🔎[" (name this) "]")))
  Object
  (toString [this] @string+)
  INamed
  (-name [this] (first @form))
  (-namespace [this] nil)
  ILookup
  (-lookup [o k]
    (-lookup o k nil))
  (-lookup [o k not-found]
    (case k
      :form form
      :operation operation
      :string string
      :string+ string+
      :fragments fragments
      :fragments+ fragments+
      (graphql-lookup o k not-found))))

(defn collect-fragments
  [found fragment]
  (as-> (conj found fragment) found
        (into found (->> (set/difference @(:fragments fragment)
                                         found)
                         (reduce collect-fragments found)))))

(defn emit
  "Returns GraphQL record for hiccup vector"
  [operation form]
  (let [form (delay (if (fn? form) (form) form))
        emitted-data (delay
                      (binding [*fragments* (atom #{})]
                        (let [string (x/emit {:emit-vec emit-vec
                                              :emit-nonvec emit-nonvec} @form)]
                          {:string string
                           :fragments @*fragments*})))
        fragments (delay
                   (-> @emitted-data
                       (get :fragments)))
        fragments+ (delay
                    (->> @fragments
                         (reduce collect-fragments #{})))
        string (delay
                (-> @emitted-data
                    (get :string)))
        string+ (delay
                 (str @string
                      (->> @fragments+
                           (map (comp deref :string))
                           (cons nil)
                           (str/join \newline))))]
    (->GraphQL form
               operation
               string
               string+
               fragments
               fragments+)))

(defn directive
  ([name body]
   (directive name {} body))
  ([name props body]
   (update (vec-wrap body) 1 assoc-in [:gql/directives name] props)))

(defn top-level-keys [query]
  (->> @(:form query)
       (drop 2)
       (mapv first)))

(defn options [query]
  (->> @(:form query)
       (second)))

(comment
 (let [include+ (partial directive :include)]
   (println (str (query someQuery [$el :String
                                   :or {$el "Hello"}]
                        [:me
                         (include+ {:if $el}
                                   [:... :name])])))))

