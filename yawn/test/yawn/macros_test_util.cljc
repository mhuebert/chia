(ns yawn.macros-test-util
  (:require [yawn.macros :as m]))

(defn util-fn []
  [:in-util (m/stage-info)])

