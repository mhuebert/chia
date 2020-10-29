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
                                     (assoc 0 'hicada.interpret/Fragment)
                                     (as-js)))})

;; TODO: We should take &env around everything and also expect it as an argument.
(def default-options {:warn-on-interpretation? true
                      :create-element 'hicada.interpret/createElement
                      :interpret/form 'hicada.interpret/form
                      :interpret/props 'hicada.interpret/props
                      :interpret/class 'hicada.interpret/class-string
                      :interpret/update-class! 'hicada.interpret/update-class!
                      :inlineable-types '#{number
                                           string
                                           function
                                           js}
                      :tag-handlers default-tag-handlers})

(defn with-defaults [options]
  (-> default-options
      (merge options)
      (assoc :tag-handlers (merge (:tag-handlers default-options)
                                  (:tag-handlers options)))))

(def ^:dynamic *options* default-options)
