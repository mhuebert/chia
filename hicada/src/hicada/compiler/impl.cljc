(ns hicada.compiler.impl
  (:require [hicada.util :as util]))

(defn form-op
  "Get the name of the supplied form."
  [form]
  (when (seq? form)
    (some-> (first form)
            (util/guard symbol?))))

(defmulti wrap-return*
          "Pre-compile certain standard forms, where possible."
          form-op)

(defmethod wrap-return* :default
  [form]
  nil)

(defn wrap-return
  "Wraps return clauses of common Clojure operators with `f`"
  [form f]
  (when-let [op (doto (form-op form) prn)]
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
      (wrap-return* form))))
