(ns chia.graphql
  (:require [chia.util :as u]
            [chia.x-vec :as x]
            [clojure.string :as str]))

(defn options [form]
  (some-> (second form)
          (u/guard map?)))

(defn get-operation [form]
  (some-> (first form)
          (namespace)
          (as-> x ({"..." "fragment"} x x))
          (str/lower-case)
          (u/guard #{"query"
                     "mutation"
                     "fragment"
                     "..."})
          (keyword)))

(defn children
  [form]
  (-> (x/parse-vec form)
      second))

(defn vec-wrap [x]
  (cond (keyword? x) [x {}]
        (and (vector? x) (not (map? (second x))))
        (into [(first x) {}] (rest x))
        :else x))

(defn directive
  ([name body]
   (directive name {} body))
  ([name props body]
   (update (vec-wrap body) 1 assoc-in [:graphql/directives name] props)))

#_(comment
   (let [include+ (with-xkeys directive :include)]
     (println (str (query someQuery [$el :String
                                     :or {$el "Hello"}]
                          [:me
                           (include+ {:if $el}
                                     [:... :name])]))))

   (defn emit
     "Returns GraphQL record for hiccup vector"
     [form]
     (let [op-type (get-operation form)
           form (delay (if (fn? form) (form) form))
           emitted-data (delay
                         (string/emit* @form))
           fragments (delay
                      (:fragments @emitted-data))
           fragments+ (delay
                       (->> @fragments
                            (reduce string/collect-fragments #{})))
           string (delay
                   (:string @emitted-data))
           string+ (delay
                    (str @string
                         (->> @fragments+
                              (map (comp deref :string))
                              (cons nil)
                              (str/join \newline))))]
       (->GraphQL form
                  op-type
                  string
                  string+
                  fragments
                  fragments+))))

