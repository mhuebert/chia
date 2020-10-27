(ns hicada.normalize
  "
  Mostly from sablono + hiccup project.
  "
  (:require
    [hicada.util :as util]))

(defn map-of-some
  "Returns map for non-nil values"
  [& kvs]
  (reduce
    (fn [m [k v]]
      (cond-> m
              (some? v)
              (assoc k v)))
    {} (partition 2 kvs)))

(defn kw->name
  [x]
  (cond-> x (keyword? x) name))

(defn children-as-list
  "Normalize the children of a HTML element."
  [x]
  (cond (nil? x) x
        (vector? x) (list x)
        (sequential? x) x
        :else (list x)))

(defn conj-class [classes class-string]
  (cond (nil? classes) class-string
        (vector? classes) (conj classes class-string)
        (string? classes) (str classes " " class-string)
        :else `(str (~'hicada.interpreter/classes-string ~classes)
                    " "
                    ~class-string)))

(defn hiccup-vec
  "Given:
  [:div.x.y#id (other)]
  Returns:
  [:div {:id \"id\"
         :class [\"x\" \"y\"]}
    (other)]"
  [[tag & body]]
  (let [[tag id class-string] (if (or (keyword? tag)
                                      (string? tag))
                                (util/parse-tag (name tag))
                                [tag nil nil])
        options {:compile-children? (string? tag)}]
    ;; TODO
    ;; what about a dynamic keyword?
    (if-let [map-props (util/guard (first body) map?)]
      [tag
       (cond-> map-props
               id (assoc :id id)
               class-string (update :class conj-class class-string))
       (children-as-list (next body))
       options]
      (if-let [map-props-interpreted (util/guard (first body) (comp :props meta))]
        [tag
         `(~'hicada.interpreter/props ~id ~class-string ~map-props-interpreted)
         (children-as-list body)
         options]
        [tag
         (map-of-some :id id :class class-string)
         (children-as-list body)
         options]))))


(comment
  (hiccup-vec [:div#foo 'a])
  (hiccup-vec [:div.a#foo])
  (hiccup-vec [:h1.b {:className "a"}])
  (hiccup-vec '[:div (for [x xs] [:span 1])]))

