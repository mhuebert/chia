(ns hicada.compiler.env
  (:require [hicada.util :as util]))

(def default-handlers {:> (fn [[_ klass attrs & children]]
                            [klass attrs children])
                       :<> (fn [form]
                             (if-let [props (util/guard (nth form 1 nil) map?)]
                               ['js/React.Fragment props (subvec form 2)]
                               ['js/React.Fragment {} (subvec form 1)]))})

;; TODO: We should take &env around everything and also expect it as an argument.
(def default-options {:inline? false
                      :array-children? false
                      :emit-fn nil
                      :warn-on-interpretation? true
                      ;; If you also want to camelcase string map keys, add string? here:
                      :camelcase-key-pred (some-fn keyword? symbol?)
                      :create-element 'js/React.createElement
                      :inlineable-types #{'number 'string}
                      :interpret 'hicada.interpreter/interpret
                      :handlers default-handlers})

(defn with-defaults [options]
  (-> default-options
      (merge options)
      (assoc :handlers (merge (:handlers default-options)
                              (:handlers options)))))

(def ^:dynamic *options* default-options)
