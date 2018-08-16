(ns chia.routing.aux
  (:require [clojure.string :as str]
            [chia.routing.util :as u]))

(defn parse-path
  "Given a `path` string, returns map of {<route-name>, <path>}

  :root will contain the base route.

  Uses the Angular auxiliary route format of http://root-path(name:path//name:path).

  Ignores query parameters."
  [path]
  (let [[_ root-string auxiliary-string] (re-find #"([^(?]*)(?:\(([^(]+)\))?(\?.*)?" path)]
    (merge {:root (u/ensure-leading-char "/" root-string)}
           (->> (when auxiliary-string
                  (str/split auxiliary-string "//"))
                (reduce (fn [m path]
                          (let [[_ router path] (re-find #"([^:]+)(?::?(.*))" path)]
                            (assoc m (keyword router) (u/ensure-leading-char \/ path)))) {})))))

(defn emit-routes
  "Given a map of the form {<route-name>, <path>},
   emits a list of auxiliary routes, wrapped in parentheses.

   :root should contain the base route, if any.

   e.g. /hello(nav:details/edit//drawer:profile/photo)"
  [routes]
  (str (some->> (dissoc routes :root)
                (keep (fn [[router path]]
                        (assert (string? path))
                        (when path
                          (str (name router)
                               (some->> path
                                        (u/trim-leading-char \/)
                                        (u/some-str)
                                        (str ":"))))))
                (seq)
                (str/join "//")
                (u/wrap "()"))
       (u/ensure-leading-char \/ (:root routes))))