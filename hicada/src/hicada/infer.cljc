(ns hicada.infer
  (:require [cljs.analyzer :as ana]
            [hicada.compiler.env :as env])
  #?(:cljs (:require-macros cljs.analyzer.macros)))

(defn infer-type
  [form env]
  (ana/infer-tag env
                 (#?(:clj ana/no-warn :cljs cljs.analyzer.macros/no-warn)
                   (ana/analyze env form))))

(defmacro inferred-type [x]
  (list 'quote (infer-type x &env)))

(defmacro maybe-interpret
  "Macro that wraps `expr` with interpreter call, if it cannot be inlined based on inferred type."
  [expr]
  (let [{:keys [inlineable-types
                warn-on-interpretation?
                interpret]} env/*options*
        tag (infer-type expr &env)
        expr-meta (meta expr)]
    (if (or (contains? inlineable-types tag)
            (:element expr-meta))
      expr
      (binding [*out* *err*]
        (when (and warn-on-interpretation?
                   (not (:hiccup expr-meta)))
          (println "WARNING: interpreting by default, please specify ^:hiccup or ^:element")
          (prn expr)
          (println "Inferred tag was:" tag)
          (let [{:keys [line file]} (meta expr)]
            (when (and line file)
              (println (str file ":" line))))
          (println ""))
        `(~interpret ~expr)))))
