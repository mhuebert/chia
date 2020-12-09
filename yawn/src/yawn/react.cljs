(ns yawn.react
  (:require ["react" :as React]))

;; https://github.com/lilactown/helix/commit/02b1e2072fb8283bd14575bd3686c94d9b15610f
(def react React)

(def Fragment React/Fragment)
(def Suspense React/Suspense)
(def ^js createElement React/createElement)
