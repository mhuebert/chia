(ns hicada.react
  (:require ["react" :as react]))

;; https://github.com/lilactown/helix/commit/02b1e2072fb8283bd14575bd3686c94d9b15610f
(defn get-react [] react)

(def Fragment react/Fragment)
(def Suspense react/Suspense)
(def ^js createElement react/createElement)
