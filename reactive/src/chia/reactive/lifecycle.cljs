(ns chia.reactive.lifecycle)

(defprotocol IDispose
  (on-dispose [context key f]
              "Register a callback to be fired when context is disposed.")
  (dispose! [context]))

(defonce ^:dynamic *owner* nil)