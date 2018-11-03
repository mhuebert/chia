(ns chia.view.view-specs
  (:require [chia.view.util :as util]
            [clojure.string :as string]
            ["react" :as react]))

(def spec-registry
  "Global registry for view specs"
  {})

(defn defspecs
  "Define a view spec"
  [specs]
  (set! spec-registry (merge spec-registry (reduce-kv (fn [m k v]
                                                        (cond-> m
                                                                (not (map? v)) (assoc k {:spec v
                                                                                         :spec-name k}))) specs specs))))
(def ReactElement? react/isValidElement)

(def Hiccup? #(and (vector? %)
                   (keyword? (first %))))

(def SVG? #(and (Hiccup? %)
                (string/starts-with? (name (first %)) "svg")))

(def Element? (util/any-pred
               nil?
               ReactElement?
               Hiccup?
               string?))

(def builtins [[:Boolean boolean?]
               [:String string?]
               [:Number number?]
               [:Function fn?]
               [:Map map?]
               [:Vector vector?]
               [:Element Element?]
               [:Hiccup Hiccup?]
               [:SVG SVG?]
               [:Object object?]
               [:Keyword keyword?]])

(defspecs (into {} builtins))
(def ^:private spec-kinds (reduce (fn [m [name pred]] (assoc m pred name)) {} builtins))

(defn resolve-spec
  "Resolves a spec. Keywords are looked up in the spec registry recursively until a function or set is found.
  If a map's :spec is a namespaced keyword, it is resolved and merged (without overriding existing keys)"
  [k]
  (cond (keyword? k) (resolve-spec (or (get spec-registry k)
                                       (throw (js/Error (str "View spec not registered: " k)))))
        (set? k) {:spec k
                  :spec-name :Set}
        (fn? k) {:spec k}
        (map? k) (let [spec (get k :spec)]
                   (if (or (fn? spec)
                           (set? spec))
                     k
                     (merge k (resolve-spec spec))))
        :else (throw (js/Error (str "Invalid spec: " k)))))

(defn spec-kind [{:keys [spec-name spec]}]
  (or (get spec-kinds spec)
      (if (set? spec) :Set
                      spec-name)))

(defn normalize-props-map
  "Resolves specs in map"
  [{:keys [keys
           keys-req] :as prop-specs}]
  (assert (and (or (nil? keys) (coll? keys))
               (or (nil? keys-req) (coll? keys-req))))
  (let [required (set keys-req)]
    (as-> (-> prop-specs
              (dissoc :keys :keys-req)) prop-specs

          ;; resolve specs
          (reduce-kv (fn [m k v]
                       (assoc m k (resolve-spec v)))
                     prop-specs prop-specs)

          ;; expand :keys into resolved map entries
          (reduce (fn [m k] (assoc m (keyword (name k)) (resolve-spec k)))
                  prop-specs keys)

          ;; expand :keys-req into resolved map entries
          (reduce (fn [m k]
                    (assoc m (keyword (name k)) (assoc (resolve-spec k) :required true)))
                  prop-specs required)

          ;; collect defaults and consumed keys
          (reduce-kv (fn [m k v]
                       (let [{:keys [default pass-through] :as spec} v]
                         (cond-> (assoc m k spec)
                                 (not pass-through) (update :props/consumed conj k)
                                 default (assoc-in [:props/defaults k] default))))
                     (assoc prop-specs
                       :props/consumed #{})
                     prop-specs))))

(defn resolve-spec-vector
  "Resolves specs in vector"
  [specs]
  (when specs (let [[req opt] (split-with (partial not= :&) specs)]
                {:req (map resolve-spec req)
                 :&more (some-> (second opt) (resolve-spec))})))

(defn validate-spec [k {:keys [required spec spec-name] :as spec-map} value]
  (when (and spec-map (not (fn? spec)) (not (set? spec)))
    (prn :invalid-spec? k {:spec spec
                           :fn? (fn? spec)
                           :m spec-map}))
  (if (nil? value)
    (when required (throw (js/Error (str "Prop is required: " k))))
    (when (and spec (not (spec value)))
      (throw (js/Error (str "Validation failed: " [k {:should-be (or spec-name spec)
                                                      :actual-value value}]))))))

(defn validate-props [display-name
                      {:keys [keys-req]
                       :as prop-specs} props]
  (when (or props
            (seq keys-req))
    (try
      (doseq [k (into keys-req (filterv #(not (#{"props" "spec"} (namespace %))) (keys props)))]
        (validate-spec k (get prop-specs k) (get props k)))
      (catch js/Error e
        (js/console.error display-name e)
        (throw (js/Error.)))))
  props)

(defn validate-children [display-name {:keys [req &more] :as children-spec} children]
  (when children-spec
    (try
      (let [children (util/flatten-seqs children)]
        (loop [remaining-req req
               remaining-children children
               i 0]
          (if (empty? remaining-req)
            (when-not (empty? remaining-children)
              (if &more
                (doseq [child remaining-children]
                  (validate-spec :children-& &more child))
                (throw (js/Error (str "Expected fewer children. Provided " (count children) ", expected " (count req) (when &more " or more") ".")))))
            (if (empty? remaining-children)
              (throw (js/Error (str "Expected more children in " display-name ". Provided " (count children) ", expected " (count req) (when &more " or more") ".")))
              (do (validate-spec (keyword (str "children-" i))
                                 (first remaining-req)
                                 (first remaining-children))
                  (recur (rest remaining-req)
                         (rest remaining-children)
                         (inc i)))))))
      (catch js/Error e
        (.error js/console (str "Error validating children in " display-name))
        (throw (js/Error e)))))
  children)