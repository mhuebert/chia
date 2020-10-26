(ns hicada.normalize
  "
  Mostly from sablono + hiccup project.
  "
  (:require
    [hicada.util :as util]))

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

(defn strip-css
  "Strip the # and . characters from the beginning of `s`."
  [s]
  (cond-> s
          (and (some? s)
               (or (.startsWith s ".")
                   (.startsWith s "#")))
          (subs 1)))

(comment
  (strip-css "#foo")
  (strip-css ".foo"))

(defn match-tag
  "Match `s` as a CSS tag and return a vector of tag name, CSS id and
  CSS classes."
  [s]
  (let [matches (re-seq #"[#.]?[^#.]+" (subs (str s) 1))
        [tag-name names]
        (cond (empty? matches)
              (throw (ex-info (str "Can't match CSS tag: " s) {:tag s}))
              (#{\# \.} (ffirst matches)) ;; shorthand for div
              ["div" matches]
              :default
              [(first matches) (rest matches)])]
    [(keyword tag-name)
     (first (map strip-css (filter #(= \# (first %1)) names)))
     (vec (map strip-css (filter #(= \. (first %1)) names)))]))

(comment
  (match-tag :.foo.bar#some-id)
  (match-tag :foo/span.foo.bar#some-id.hi))

(defmacro t [expr]
  {:list? (list? expr)
   :sym? (symbol? expr)
   :seq? (seq? expr)})

(t 'x)


(defn children
  "Normalize the children of a HTML element."
  [x]
  (when x
    (if (sequential? x)
      (cond
        (util/hiccup-vector? x) (list x)
        (and (= (count x) 1)
             (sequential? (first x))) (children (first x))
        :else x)
      (list x))))

(defn guard [x f] (when (f x) x))

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
  (let [[tag id klass] (match-tag tag)
        tag-attrs (compact-map {:id id :class klass})]
    (if-let [map-attrs (guard (first content) map?)]
      [tag
       (merge-with-class tag-attrs map-attrs)
       (children (next content))]
      [tag
       (attributes tag-attrs)
       (children content)])))

(comment
  (element [:div#foo 'a])
  (element [:div.a#foo])
  (element [:h1.b {:className "a"}]))

