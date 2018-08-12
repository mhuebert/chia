(ns chia.triple-db)

;; warning, these macros are not currently used or maintained
;
;(defmacro branch [db & body]
;  `(let [db-before# ~db]
;     (binding [~'chia.triple-db/*db* (~'atom db-before#)
;               ~'chia.triple-db.core/*mute* true
;               ~'chia.triple-db.core/*db-log* (~'atom (or (~'some-> ~'chia.triple-db.core/*db-log* ~'deref)
;                                                 {:db-before db-before#
;                                                  :db-after  db-before#
;                                                  :reports   {}}))]
;       (do ~@body)
;       @~'chia.triple-db.core/*db-log*)))
;
;(defmacro try-branch [db on-error & body]
;  `(let [db-before# ~db]
;     (try (branch db-before# ~@body)
;          (catch :default e#
;            (branch db-before# e# (~on-error e#))))))