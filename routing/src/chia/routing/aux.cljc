(ns chia.routing.aux
  (:require [chia.routing :as routing]
            [clojure.string :as str]
            [chia.util.string :as string]))

(defn parse-path
  "Given a `path` string, returns map of {<route-name>, <path>}

  :root will contain the base route.

  Uses the Angular auxiliary route format of http://root-path(name:path//name:path).

  Ignores query parameters."
  ([path]
   (parse-path {:ns nil} path))
  ([{:keys [ns]} path]
   (let [[_ root-string auxiliary-string] (re-find #"([^(?]*)(?:\(([^(]+)\))?(\?.*)?" path)]
     (merge {(keyword ns "root") (string/ensure-prefix root-string "/")}
            (->> (when auxiliary-string
                   (str/split auxiliary-string "//"))
                 (reduce (fn [m path]
                           (let [[_ router path] (re-find #"([^:]+)(?::?(.*))" path)]
                             (assoc m (keyword ns router)
                                      ((case router "root" string/ensure-prefix
                                                    string/trim-prefix)
                                       path "/")))) {}))))))

(defn wrap
  [[left right] s]
  (str left s right))

(defn emit-routes
  "Given a map of the form {<route-name>, <path>},
   emits a list of auxiliary routes, wrapped in parentheses.

   :root should contain the base route, if any.

   e.g. /hello(nav:details/edit//drawer:profile/photo)"
  ([routes] (emit-routes {:ns nil} routes))
  ([{:keys [ns]} routes]
   (let [root (keyword ns "root")
         query (keyword ns "query")]
     (str

      ;; root route
      (string/ensure-prefix (root routes "") "/")

      ;; aux routes
      (some->> (dissoc routes root query)
               (keep (fn [[router path]]
                       (assert (string? path))
                       (when path
                         (str (name router)
                              (some-> path
                                      (string/trim-prefix "/")
                                      (string/some-str)
                                      (->> (str ":")))))))
               (seq)
               (str/join "//")
               (wrap "()"))

      ;; query-string
      (some-> (query routes)
              (routing/query-string)
              (->> (str "?")))))))