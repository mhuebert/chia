(ns hicada.compiler.env)

(defn as-js [form]
  (update form 0 vary-meta assoc :tag 'js))

(defn handle-element-constructor [form]
  (-> form
      (subvec 1)
      (with-meta (meta form))
      (as-js)))

(def default-tag-handlers {:> handle-element-constructor
                           :<> (fn [form]
                                 (-> form
                                     (assoc 0 'hicada.interpreter/Fragment)
                                     (as-js)))})

;; TODO: We should take &env around everything and also expect it as an argument.
(def default-options {:inline? false
                      :array-children? false
                      :emit-fn nil
                      :warn-on-interpretation? true
                      ;; If you also want to camelcase string map keys, add string? here:
                      :camelcase-key-pred (some-fn keyword? symbol?)
                      :create-element 'hicada.interpreter/createElement
                      :inlineable-types '#{number
                                           string
                                           function
                                           js}
                      :interpret 'hicada.interpreter/interpret
                      :tag-handlers default-tag-handlers
                      :tags {:<> 'hicada.interpreter/Fragment}})

(defn with-defaults [options]
  (-> default-options
      (merge options)
      (assoc :tag-handlers (merge (:tag-handlers default-options)
                                  (:tag-handlers options)))))

(def ^:dynamic *options* default-options)
