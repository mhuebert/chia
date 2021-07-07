(ns triple.view.props
  (:require [triple.util.cond :as cond]))

(defn get-props
  "Returns props at index `i` in `form`, or a sentinel value if props were not found.
   Props can be `nil` or a Clojure map.
   Call `props?` on the result to determine if props were found.
   Props can be nil or a Clojure map."
  [form i]
  (cond/when-defined [props (nth form i js/undefined)]
    (if (or (nil? props) (object? props) (map? props))
      props
      js/undefined)))

(defn update-props [el f & args]
  {:pre [(vector? el)]}
  (let [props (get-props el 1)]
    (if (cond/defined? props)
      (assoc el 1 (apply f props args))
      (into [(el 0) (apply f {} args)] (subvec el 1)))))
