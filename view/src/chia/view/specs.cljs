(ns chia.view.specs
  (:require [cljs.spec.alpha :as s :include-macros true]
            [cljs.spec.gen.alpha :as gen]
            [chia.view.hiccup.spec]
            [chia.view :as v :include-macros true]))


(s/fdef v/defview
        :args (s/cat :name symbol?
                     :doc (s/? string?)
                     :methods (s/? (s/map-of keyword? any?))
                     :args vector?
                     :body (s/cat :side-effects (s/* any?)
                                  :render-body :chia.view.hiccup.spec/element)))

