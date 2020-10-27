(ns hicada.normalize
  "
  Mostly from sablono + hiccup project.
  "
  (:require
    [hicada.util :as util]
    [clojure.string :as str]))

(defn compact-map
  "Removes all map entries where the value of the entry is empty."
  [m]
  (reduce-kv
    (fn [m k v]
      (cond-> m
              (not (some? v))
              (dissoc k)))
    m m))

(defn kw->name
  [x]
  (cond-> x
          (keyword? x) name))

(defn vec+stringify-class
  "Normalize `class` into a vector of classes (keywords will be stringified)."
  [klass]
  (cond
    (nil? klass)
    nil

    (list? klass)
    (if (symbol? (first klass))
      [klass]
      (map kw->name klass))

    (symbol? klass)
    [klass]

    (string? klass)
    [klass]

    (keyword? klass)
    [(kw->name klass)]

    (or (set? klass)
        (sequential? klass))
    (mapv kw->name klass)

    (map? klass)
    [klass]

    :else klass))

(defn attributes
  "Normalize the :class, :class-name and :className elements"
  [attrs]
  (reduce (fn [attrs kw]
            (if-some [m (get attrs kw)]
              (-> attrs
                  (dissoc kw)
                  (update :class (fnil into []) (vec+stringify-class m)))
              attrs))
          attrs [:class :className :class-name]))

(defn merge-with-class
  "Like clojure.core/merge but concatenate :class entries."
  [m0 m1]
  (let [m0 (attributes m0)
        m1 (attributes m1)
        classes (into [] (comp (mapcat :class)) [m0 m1])]
    (cond-> (conj m0 m1)
      (not (empty? classes))
      (assoc :class classes))))

(comment
  (merge-with-class {:class "a"} {:class ["b"]}))

(defmacro t [expr]
  {:list? (list? expr)
   :sym? (symbol? expr)
   :seq? (seq? expr)})

(t 'x)

(defn unevaluated?
  "True if the expression has not been evaluated.
   - expr is a symbol? OR
   - it's something like (foo bar)"
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn children
  "Normalize the children of a HTML element."
  [x]
  (when x
    (if (sequential? x)
      (cond
        (vector? x) (list x)
        (and (= (count x) 1)
             (sequential? (first x))
             (not (unevaluated? x))) (children (first x))
        :else x)
      (list x))))

(defn conj-class [classes class-string]
  (if-not classes
    class-string
    (cond (nil? classes) class-string
          (vector? classes) (conj classes class-string)
          (string? classes) (str classes " " class-string)
          :else `(str (~'hicada.interpreter/classes-string ~classes)
                      " "
                      ~class-string)
          )))

(defn element
  "Given:
  [:div.x.y#id (other)]
  Returns:
  [:div {:id \"id\"
         :class [\"x\" \"y\"]}
    (other)]"
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (ex-info (str tag " is not a valid element name.") {:tag tag :content content})))
  (let [[tag id class-string] (if (or (keyword? tag)
                               (string? tag))
                         (util/parse-tag (name tag))
                         [tag nil nil])
        tag-props (compact-map {:id id :class class-string})]
    (if-let [map-props (util/guard (first content) map?)]
      [tag
       (cond-> map-props
               id (assoc :id id)
               class-string (update :class conj-class class-string))
       (children (next content))]
      (if-let [map-props-interpreted (util/guard (first content) (comp :props meta))]
        [tag
         `(~'hicada.interpreter/props ~id ~class-string ~map-props-interpreted)
         (children content)]
        [tag
         (attributes tag-props)
         (children content)]))))


(comment
  (element [:div#foo 'a])
  (element [:div.a#foo])
  (element [:h1.b {:className "a"}])
  (element '[:div (for [x xs] [:span 1])]))

