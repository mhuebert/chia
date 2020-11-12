(ns hicada.infer
  (:require [cljs.analyzer :as ana]
            [hicada.env :as env]
            [hicada.util :as util]
            #?(:clj cljs.analyzer.macros))
  #?(:cljs (:require-macros cljs.analyzer.macros)))

(defn infer-type
  [form env]
  (ana/infer-tag env
                 (#?(:clj ana/no-warn :cljs cljs.analyzer.macros/no-warn)
                   (ana/analyze env form))))

;; dev util
(defmacro inferred-type [x]
  (list 'quote (infer-type x &env)))

(defn skip?
  "Returns true if we can skip interpretation"
  ([form] (skip? form nil (:skip-types env/*options*)))
  ([form tag] (skip? form tag (:skip-types env/*options*)))
  ([form tag inlineable-types]
   (or (string? form)
       (number? form)
       (nil? form)
       (let [expr-meta (meta form)
             tag (or (:tag expr-meta) tag)]
         (boolean (or (contains? inlineable-types tag)
                      (:inline expr-meta)))))))

(defmacro maybe-interpret
  "Macro that wraps `expr` with interpreter call, if it cannot be skipped based on inferred type."
  [expr]
  (let [{:keys [skip-types
                warn-on-interpretation?] :as options} env/*options*
        tag (infer-type expr &env)]
    (if (skip? expr tag skip-types)
      expr
      (binding [*out* *err*]
        (when (and warn-on-interpretation?
                   (not (:interpret (meta expr))))
          (println (str "WARNING: interpreting form " (pr-str expr)
                        (let [{:keys [line file]} (meta expr)]
                          (when (and line file)
                            (str ", " file ":" line)))
                        (some->> tag (str ", ")))))
        `(~(:convert-form options) ~expr)))))
