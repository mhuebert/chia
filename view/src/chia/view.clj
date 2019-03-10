(ns chia.view
  (:refer-clojure :exclude [defn])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]))

(core/defn to-element [x]
  ;; TODO
  ;; upgrade hiccup/element to work partly at macroexpansion time
  `(~'chia.view.hiccup/element {:wrap-props ~'chia.view.props/wrap-props}
    ~x))

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
  (let [{:keys [name
                doc
                view/options
                body]
         view-name :view/name} (parse-functional-view-args args)
        {:view/keys [forward-ref?]} options
        view-fn-sym (symbol (str "-" name))
        key-fn-sym (gensym "key")
        key-fn (:key options)
        args-sym (gensym "args")]
    `(let [~key-fn-sym ~key-fn
           ~view-fn-sym (~'chia.view.hooks/-functional-render
                         {:view/name ~(str view-name)
                          :view/fn (fn ~name ~@body)
                          :view/should-update? ~(:view/should-update? options `not=)
                          :view/forward-ref? ~(:view/forward-ref? options false)})]
       (core/defn ~name [& ~args-sym]
         (let [props# (when ~(boolean (or key-fn-sym forward-ref?))
                        (~'js-obj
                         ~@(cond-> []
                                   key-fn (conj "key" `(apply ~key-fn-sym ~args-sym))
                                   forward-ref? (conj "ref" `(:ref (first ~args-sym))))))]
           (.call ~'chia.view.impl/-create-element nil ~view-fn-sym props# ~args-sym))))))