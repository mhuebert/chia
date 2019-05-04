(ns chia.view
  (:refer-clojure :exclude [defn])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [applied-science.js-interop :as j]))

(core/defn parse-functional-view-args [args]
  (let [view-map (s/conform (s/cat :name (s/? symbol?)
                                   :doc (s/? string?)
                                   :view/options (s/? map?)
                                   :body (s/+ any?))
                            args)]
    (assoc view-map :view/name
                    (symbol (name (ns-name *ns*))
                            (name (:name view-map))))))

(defmacro defn [& args]
  (let [{:keys     [name
                    doc
                    view/options
                    body]
         view-name :view/name} (parse-functional-view-args args)
        {:view/keys [forward-ref?]} options
        f-sym (symbol (str "-" name))
        keyf-sym (gensym "key")
        key-fn (:key options)
        args-sym (gensym "args")]
    `(let [~keyf-sym ~key-fn
           ~f-sym (~'chia.view/-functional-render
                   {:view/name           ~(str view-name)
                    :view/fn             (fn ~name ~@body)
                    :view/should-update? ~(:view/should-update? options `not=)
                    :view/forward-ref?   ~(:view/forward-ref? options false)})]
       (core/defn ~name [& ~args-sym]
         (let [props# (when ~(or keyf-sym forward-ref?)
                        (j/obj
                         ~@(when key-fn
                             [:key `(apply ~keyf-sym ~args-sym)])
                         ~@(when forward-ref?
                             [:ref `(:ref (first ~args-sym))])))]
           (~'chia.view/-create-element ~f-sym props# ~args-sym))))))


(defmacro defclass
  "Define a view function.

   Expects optional docstring and methods map, followed by
    the argslist and body for the render function, which should
    return a Hiccup vector or React element."
  [& args]
  `(~'chia.view.class/defclass ~@args))

