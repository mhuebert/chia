(ns chia.jss.tachyons
  (:require [chia.util :as u]))

(defn color-styles [color-map]
  (u/for-map [[n v] color-map
              :let [color-name (name n)]]
    {(str "." color-name)    {:color v}
     (str ".bg-" color-name) {:background-color v}
     (str ".b--" color-name) {:border-color v}}))

(defn- append-to-name [s postfix]
  (str (name s) postfix))

(defn with-breakpoint [breakpoint & ms]
  (let [m (apply merge ms)]
    (merge m
           {(str "@media (max-width: " breakpoint "px)")
            (u/update-keys m #(append-to-name % "-sm"))}
           {(str "@media (min-width: " (inc breakpoint) "px)")
            (u/update-keys m #(append-to-name % "-lg"))})))