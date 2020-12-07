(ns yawn.selfhost-test-util
  (:require ["react-dom/server" :as rdom-server]))

(def to-string rdom-server/renderToStaticMarkup)
