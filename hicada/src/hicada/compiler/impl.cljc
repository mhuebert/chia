(ns hicada.compiler.impl)

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (name (first form))))

(defmulti wrap-return
          "Pre-compile certain standard forms, where possible."
          form-name)

(defmethod wrap-return "do"
  [[_ & forms] f]
  `(do ~@(butlast forms) ~(f (last forms))))

(defmethod wrap-return "array"
  [[_ & forms] f]
  `(cljs.core/array ~@(mapv f forms)))

(defmethod wrap-return "let"
  [[_ bindings & body] f]
  `(let ~bindings ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "let*"
  [[_ bindings & body] f]
  `(let* ~bindings ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "letfn"
  [[_ bindings & body] f]
  `(letfn ~bindings ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "letfn*"
  [[_ bindings & body] f]
  `(letfn* ~bindings ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "list"
  [[_ & forms] f]
  `(cljs.core/array ~@(mapv f forms)))

(defmethod wrap-return "for"
  [[_ bindings body] f]
  `(for ~bindings ~(f body)))

(defmethod wrap-return "when"
  [[_ condition & body] f]
  `(when ~condition ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "when-let"
  [[_ bindings & body] f]
  `(when-let ~bindings ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "when-some"
  [[_ bindings & body] f]
  `(when-some ~bindings ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "when-not"
  [[_ condition & body] f]
  `(when-not ~condition ~@(butlast body) ~(f (last body))))

(defmethod wrap-return "if"
  [[_ condition & body] f]
  `(if ~condition ~@(doall (for [x body] (f x)))))

(defmethod wrap-return "if-let"
  [[_ bindings & body] f]
  `(if-let ~bindings ~@(doall (for [x body] (f x)))))

(defmethod wrap-return "if-not"
  [[_ condition & body] f]
  `(if-not ~condition ~@(doall (for [x body] (f x)))))

(defmethod wrap-return "if-some"
  [[_ bindings & body] f]
  `(if-some ~bindings ~@(doall (for [x body] (f x)))))

(defmethod wrap-return "case"
  [[_ v & cases] f]
  `(case ~v
     ~@(doall (mapcat
                (fn [[test hiccup]]
                  (if hiccup
                    [test (f hiccup)]
                    [(f test)]))
                (partition-all 2 cases)))))

(defmethod wrap-return "condp"
  [[_ f v & cases] f]
  `(condp ~f ~v
     ~@(doall (mapcat
                (fn [[test hiccup]]
                  (if hiccup
                    [test (f hiccup)]
                    [(f test)]))
                (partition-all 2 cases)))))

(defmethod wrap-return "cond"
  [[_ & clauses] f]
  `(cond ~@(mapcat
             (fn [[check expr]] [check (f expr)])
             (partition 2 clauses))))

(defmethod wrap-return :default [_ _] nil)
