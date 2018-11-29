(ns chia.graphql.string
  (:require [chia.x-vec :as x]
            [chia.util :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [chia.graphql :as g]))

(def ^:dynamic *fragments* nil)
(def ^:dynamic *add-typename* true)

(def spacer "  ")

(defn depth-spacing
  ([] (depth-spacing 0))
  ([n]
   (apply str (take (+ x/*depth* n) (repeat spacer)))))

(defn emit-value [x]
  (cond (string? x) (str \" x \")
        (or (keyword? x) (symbol? x)) (let [[original-name base-name required-inner? collection? required-outer?] (re-matches #"([^+!]+)(!)?(\+)?(!)?" (name x))]
                                        (if collection?
                                          (str "[" base-name (when required-inner? "!") "]"
                                               (when required-outer? "!"))
                                          original-name))
        (boolean? x) (str x)
        (number? x) x
        (nil? x) "null"
        (vector? x) (str "[" (str/join ", " (mapv emit-value x)) "]")
        (map? x)
        (let [props? (::props (meta x))
              [lbracket rbracket] (if props? ["(" ")"] ["{" "}"])]
          (when-let [fields (->> x
                                 (keep (fn [[key value]]
                                         (when-not (and (keyword? key) (= "graphql" (namespace key)))
                                           (str (name key) ": " (emit-value value)))))
                                 (seq))]
            (str lbracket (str/join ", " fields) rbracket)))))

(defn emit-children
  ([children]
   (let [spacing (depth-spacing)]
     (str/join (str \newline spacing) children)))
  ([[left right] children]
   (when (seq children)
     (str " "
          left
          \newline
          (depth-spacing)
          (emit-children children)
          (str \newline (depth-spacing -1)
               right)))))

(comment
 (= (vec-wrap :x) [:x {}])
 (= (vec-wrap [:x]) [:x {}])
 (= (vec-wrap [:x :y]) [:x {} :y]))

(defn update-last [v f & args]
  (apply update v (dec (count v)) f args))

#_[
   :... :a :b splice (no name & no target)

   :.../fields :a :b splice (no target)

   :... {:on :Profile} :a :b inline

   :.../fields {:on :Profile} :a :b ref

   :ref name &&
   ]


(defn emit-vec [tag {:as props
                     :keys [graphql/directives
                            graphql/alias-of
                            graphql/emit-fragment]} children]
  (let [operation (g/get-operation [tag])
        fragment-behaviour (when (or (= operation :fragment)
                                     (= tag :...))
                             (let [fragment-name (when (namespace tag)
                                                   (name tag))
                                   fragment-target (:on props)]

                               (cond emit-fragment :toplevel
                                     (and fragment-name
                                          fragment-target) :ref
                                     fragment-target :inline
                                     :else :splice)))]
    (case fragment-behaviour
      :ref (do
             (some-> *fragments* (swap! conj (into [tag (assoc props :graphql/emit-fragment true)] children)))
             (str "..." (name tag)))
      :splice (binding [x/*depth* (dec x/*depth*)]
                (emit-children children))
      (let [{:as props
             :keys [graphql/on]} (cond-> props
                                         fragment-behaviour
                                         (set/rename-keys {:on :graphql/on}))]
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
                 (emit-children "{}" (cond-> children
                                             *add-typename* (conj "__typename")))))))))

(defn emit-nonvec [x]
  (cond (or (nil? x)
            (string? x)) x

        (keyword? x) (name x)

        (number? x) (str x)

        :else (do
                (prn :nonvec-emit-error x)
                (throw (js/Error. (str "Not a keyword or number: " x ", " (type x)))))))

(defn emit* [form]
  (binding [*fragments* (atom #{})]
    (let [string (x/emit {:emit-vec emit-vec
                          :emit-nonvec emit-nonvec} form)]
      {:form form
       :string string
       :fragments @*fragments*})))

(defn collect-fragments
  ([emitted-form]
   (collect-fragments #{} emitted-form))
  ([found {:as emitted-form
           :keys [fragments]}]
   (let [found (conj found emitted-form)
         next-fragments (set (mapv emit* fragments))]
     (->> (set/difference next-fragments
                          found)
          (reduce collect-fragments found)
          (into found)))))

(defn emit [form]
  (let [emitted-form (emit* form)
        fragments+ (collect-fragments emitted-form)
        string+ (->> fragments+
                     (mapv :string)
                     (cons nil)
                     (str/join \newline))]
    {:form form

     :string (:string emitted-form)
     :fragments (:fragments emitted-form)

     :string+ string+
     :fragments+ fragments+}))