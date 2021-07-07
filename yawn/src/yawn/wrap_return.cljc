(ns yawn.wrap-return
  "Macro utility for wrapping return values.

   This has been copied and modified from weavejester/hiccup & r0man/sablono:
   - https://github.com/weavejester/hiccup/blob/master/src/hiccup/compiler.clj#L176
   - https://github.com/r0man/sablono/blob/master/src/sablono/compiler.clj#L138")

(defn guard [x f] (when (f x) x))

(defn operator
  "When form is a seq beginning with a symbol, returns that symbol"
  [form]
  (some-> (guard form seq?)
          first
          (guard symbol?)))

(defmulti wrap-return*
          "multimethod for extending `wrap-return`. "
          (comp operator (fn [form f options] (operator form))))

(defmethod wrap-return* :default
  [form f options]
  nil)

(defn wrap-return
  "Wraps return clauses of common Clojure operators with `f`.
   Returns nil if no match is found (to allow consumers to handle this case differently).
   Csn be extended via the `wrap-return*` multimethod."
  [form f options]
  (when-let [op (operator form)]
    (or (wrap-return* form f options)
        (case (name op)
          "do"
          `(do ~@(butlast (rest form)) ~(f (last form)))

          ("array"
            "list")
          `(cljs.core/array ~@(mapv f (rest form)))

          ("let"
            "let*"
            "letfn"
            "letfn*")
          (let [[_ bindings & body] form]
            `(~op ~bindings ~@(butlast body) ~(f (last body))))

          "for"
          (let [[_ bindings body] form]
            `(for ~bindings ~(f body)))

          ("when"
            "when-not"
            "when-let"
            "when-some")
          (let [[_ condition & body] form]
            `(~op ~condition ~@(butlast body) ~(f (last body))))

          ("if"
            "if-let"
            "if-not"
            "if-some")
          (let [[_ condition then else] form]
            `(~op ~condition
               `(do ~@(butlast then) ~(f (last then)))
               `(do ~@(butlast else) ~(f (last else)))))

          "case"
          (let [[_ v & cases] form]
            `(case ~v
               ~@(doall (mapcat
                          (fn [[test hiccup]]
                            (if hiccup
                              [test (f hiccup)]
                              [(f test)]))
                          (partition-all 2 cases)))))

          "condp"
          (let [[_ f v & cases] form]
            `(condp ~f ~v
               ~@(doall (mapcat
                          (fn [[test hiccup]]
                            (if hiccup
                              [test (f hiccup)]
                              [(f test)]))
                          (partition-all 2 cases)))))

          "cond"
          (let [[_ & clauses] form]
            `(cond ~@(mapcat
                       (fn [[check expr]] [check (f expr)])
                       (partition 2 clauses))))
          nil))))
