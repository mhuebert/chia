(ns hicada.util
  #?(:cljs (:require [applied-science.js-interop :as j]))
  #?(:cljs (:require-macros hicada.util)))

(defn primitive?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (or (string? x)
      (keyword? x)
      (number? x)
      (nil? x)))

(defn guard [x f] (when (f x) x))

(defn memo-by-string
  "Memoizes f, using cljs object lookups (for perf)"
  [f]
  #?(:clj
     (memoize f)

     :cljs
     (let [state (j/lit {:value {}})]
       (-> (fn [^string x]
             (let [cache (j/!get state :value)]
               (j/!get cache x
                       (let [v (f x)]
                         (j/!set cache x v)
                         v))))

           ;; clear the memory
           (j/assoc! :clear #(j/!set state :value #js{}))))))

#?(:cljs
   (defn memo-clear! [memoized-fn]
     (j/call memoized-fn :clear)))

'(defmacro case [& {:keys [cljs clj]}]
   (if (contains? &env '&env)                               ;; literally inside defmacro
     `(if (:ns ~'&env)                                      ;; target is cljs
        ~cljs
        ~clj)

     ;; ...not inside defmacro
     (if #?(:clj (:ns &env)                                 ;; :ns is inside &env... target is cljs..
            :cljs true)
       cljs
       clj)))

(defmacro casetime
  "in cljs/selfhost - evaluates deftime when evaluating a macros namespace
   in cljs/jvm      - evaluates deftime when evaluating a macros namespace
   in clj           - evaluates deftime when called from within a macro (lexical scope)"
  [& {:keys [deftime
             usetime]}]
  (if #?(:clj  (contains? &env '&env)
         :cljs (re-matches #".*\$macros" (name (ns-name *ns*))))
    deftime
    usetime))

(defmacro if-cljs-macrotime [then else]
  (if (re-matches #".*\$macros" (name (ns-name *ns*)))
    then
    else))

(comment

  (defn a-fn []
    (casetime :deftime :deftime
              :usetime :usetime))

  (defmacro a-macro []
    (casetime :deftime :deftime
              :usetime :usetime))

  (a-fn)
  (a-macro)
  )
