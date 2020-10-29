(ns hicada.infer
  (:require [cljs.analyzer :as ana]
            [hicada.compiler.env :as env]
            [hicada.util :as util])
  #?(:cljs (:require-macros cljs.analyzer.macros)))

(defn infer-type
  [form env]
  (ana/infer-tag env
                 (#?(:clj ana/no-warn :cljs cljs.analyzer.macros/no-warn)
                   (ana/analyze env form))))

(defmacro inferred-type [x]
  (list 'quote (infer-type x &env)))

(defn inline?
  "Returns true if we can skip interpretation"
  ([form] (inline? form nil (:inlineable-types env/*options*)))
  ([form tag] (inline? form tag (:inlineable-types env/*options*)))
  ([form tag inlineable-types]
   (or (string? form)
       (number? form)
       (nil? form)
       (let [expr-meta (meta form)
             tag (or (:tag expr-meta) tag)]
         (tap> {:form form
                :meta (meta form)
                :tag tag})
         (boolean (or (contains? inlineable-types tag)
                      (:inline expr-meta)))))))

(defmacro maybe-interpret
  "Macro that wraps `expr` with interpreter call, if it cannot be inlined based on inferred type."
  [expr]
  (let [{:keys [inlineable-types
                warn-on-interpretation?
                interpret]} env/*options*
        tag (infer-type expr &env)]
    (if (inline? expr tag inlineable-types)
      expr
      (binding [*out* *err*]
        (when (and warn-on-interpretation?
                   (not (:interpret (meta expr))))
          (println "WARNING: interpreting by default, please specify ^:interpret or ^:inline")
          (prn expr)
          (println "Inferred tag was:" tag)
          (let [{:keys [line file]} (meta expr)]
            (when (and line file)
              (println (str file ":" line))))
          (println ""))
        `(~interpret ~expr)))))
