(ns hicada.normalize
  "
  Mostly from sablono + hiccup project.
  "
  (:require
    [hicada.util :as util]))

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
        :else `(str (~'hicada.compiler/ensure-class-string ~classes)
                    " "
                    ~class-string)))

(defn props-mode [props]
  (or (when (map? props) :map)
      (when (nil? props) :nil)
      (when (or (seq? props) (symbol? props))
        (let [props-meta (meta props)]
          (cond (:props props-meta) :dynamic-map
                (#{'object
                   'js} (:tag props-meta)) :js-object)))
      :no-props))

(defn hiccup-vec
  "Given:
  [:div.x.y#id (other)]
  Returns:
  [:div {:id \"id\"
         :class [\"x\" \"y\"]}
    (other)]"
  [[tag & body :as vec]]
  (let [js-element? (or (string? tag)
                        (keyword? tag)
                        (and (symbol? tag)
                             (= 'js (:tag (meta tag)))))]
    (if js-element?
      (let [[tag id class-string] (if (or (keyword? tag)
                                          (string? tag))
                                    (util/parse-tag (name tag))
                                    [tag nil nil])
            props (first body)
            mode (props-mode props)
            props? (not= mode :no-props)]
        [tag
         (when props? props)
         (children-as-list (cond-> body props? next))
         (merge
           {:js-element? true
            :id id
            :class-string class-string
            :prop-mode mode}
           (select-keys (meta vec) [:ref :key]))])
      [tag nil body (select-keys (meta vec) [:key :ref])])))


(comment
  (hiccup-vec [:div#foo 'a])
  (hiccup-vec [:div.a#foo])
  (hiccup-vec [:h1.b {:className "a"}])
  (hiccup-vec '[:div (for [x xs] [:span 1])]))

