(ns hicada.infer
  (:require [cljs.analyzer :as ana]
            #?(:cljs cljs.analyzer.macros)
            [hicada.compiler.env :as env]))

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
        tag (infer-type expr &env)]
    (if (contains? inlineable-types tag)
      expr
      (binding [*out* *err*]
        (when warn-on-interpretation?
          (println "WARNING: interpreting by default, please specify ^:inline or ^:interpret")
          (prn expr)
          (println "Inferred tag was:" tag)
          (let [{:keys [line file]} (meta expr)]
            (when (and line file)
              (println (str file ":" line))))
          (println ""))
        `(~interpret ~expr)))))
