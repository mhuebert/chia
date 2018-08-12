(ns chia.static.page-specs
  (:require [chia.static.page :as page]
            [clojure.spec.alpha :as s]))


(s/def ::page/title string?)
(s/def ::page/lang string?)
(s/def ::page/charset string?)

;; styles can be strings (inline styles) or props to pass to a :link tag,
;; where `:rel "stylesheet"` is provided.
(s/def ::page/href string?)
(s/def ::page/style-props (s/keys
                      :req-un [::href]))
(s/def ::page/styles (s/coll-of (s/or :inline-style string?
                                 :href ::page/href)))

;; `meta` attributes are passed as a map, and expanded to the appropriate tags.
(s/def ::page/meta (s/map-of keyword?
                        string?))

;; scripts can either be strings (for javascript to eval)
;; or maps (props passed to the :script element)
(s/def ::page/src string?)
(s/def ::page/script-props (s/keys
                       :req-un [::src]))
(s/def ::page/scripts (s/coll-of (s/or :inline-js string?
                                  :script-props map?)))

;; scripts are differentiated by their position in the document.
(s/def :scripts/head ::page/scripts)
(s/def :scripts/body ::page/scripts)

;; `body` and `head` are lists of elements.
;;  keywords are wrapped in vectors.
(s/def ::page/element (s/or :tag keyword?
                       :element vector?))
(s/def ::page/body (s/coll-of ::page/element))
(s/def ::page/head (s/coll-of ::page/element))

(s/def ::page/html-page-props
  (s/keys :opt-un [::title
                   ::page/lang
                   ::page/charset
                   ::page/styles
                   ::page/meta
                   ::page/body
                   ::page/head]
          :opt [:scripts/head
                :scripts/body]))

(s/fdef page/html-page
        :args (s/alt :one-arity (s/cat :page-props ::page/html-page-props)
                     :two-arity (s/cat :title ::page/title
                                       :page-props ::page/html-page-props))
        :ret string?)