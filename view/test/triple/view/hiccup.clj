(ns triple.view.hiccup)

(defmacro register-tag [k handler]
  `(j/!set
     ~'triple.view.hiccup/tag-handlers
     ~(name k)
     ~handler))

(defmacro register-tags [m]
  `(do
     ~@(for [[k handler] (seq m)]
         `(register-tag ~k ~handler))))
