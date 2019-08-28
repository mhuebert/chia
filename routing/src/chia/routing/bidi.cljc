(ns chia.routing.bidi
  (:require [bidi.bidi :as bidi]))

(defn- flat-map-seq
  [map-args]
  (into []
        (comp (remove (comp nil? second))
              (mapcat identity))
        map-args))

(defn path-for [routes handler params]
  (apply bidi/path-for routes handler (flat-map-seq params)))