(ns chia.cell.runtime)

(defprotocol IDispose
  "Cells are designed to be used within interactive interfaces where users re-evaluate
   code frequently. Implement the IDispose protocol on an editor context to control the
    'disposal' of side-effects like intervals when code is (re)-evaluated."
  (on-dispose [context f] "Register a callback to be fired when context is disposed.")
  (-dispose! [context]))

(defn dispose! [value]
  (when (satisfies? IDispose value)
    (-dispose! value)))

(defprotocol IHandleError
  (handle-error [this e]))

(defn default-runtime []
  (let [-context-state (volatile! {:on-dispose #{}})]
    (reify
      IDispose
      (on-dispose [_ f]
        (vswap! -context-state update :on-dispose conj f))
      (-dispose! [_]
        (doseq [f (:on-dispose @-context-state)]
          (f))
        (vswap! -context-state update :on-dispose empty))
      IHandleError
      (handle-error [this e] (throw e)))))

(defonce ^:dynamic *runtime* (default-runtime))
(defonce ^:dynamic *reload* false)