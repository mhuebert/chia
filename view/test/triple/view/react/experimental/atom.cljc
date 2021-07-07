(ns triple.view.react.experimental.atom
  (:refer-clojure :exclude [use])
  (:require ["react" :as react]
            [applied-science.js-interop :as j]
            [triple.view.react.experimental.mutable-source :as mutable]
            [triple.view.react.hooks :as hooks]))

(defonce ^:private listener-counter (volatile! 0))

(defn- subscribe! [instance cb]
  (let [n (vswap! listener-counter inc)]
    (-add-watch instance n cb)
    #(-remove-watch instance n)))

(deftype ReactiveAtom [^:mutable state
                       listeners
                       ^:mutable source
                       get-snapshot]
  Object
  (equiv [this other]
    (-equiv this other))

  IAtom

  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [_]
    (if (hooks/current-view?)
      (mutable/use source get-snapshot subscribe!)
      state))

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [f (.values js/Object listeners)]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (j/!set listeners key f)
    this)
  (-remove-watch [this key]
    (js-delete listeners key)
    this)

  IReset
  (-reset! [this new-value]
    (let [old-value state]
      (set! state new-value)
      (when-not (nil? listeners)
        (-notify-watches this old-value new-value))
      new-value))
  ISwap
  (-swap! [this f] (reset! this (f state)))
  (-swap! [this f x] (-reset! this (f state x)))
  (-swap! [this f x y] (-reset! this (f state x y)))
  (-swap! [this f x y more] (-reset! this (apply f state x y more)))
  IHash
  (-hash [this] (goog/getUid this)))

(defn create
  "Returns an atom backed by a React mutable source."
  [initial-val]
  (let [ratom (ReactiveAtom. initial-val #js{} nil nil)
        get-snapshot #(.-state ratom)
        get-version get-snapshot]
    (j/!set ratom
            .-get-snapshot get-snapshot
            .-source (mutable/create ratom get-version))))

(defn use
  "[hook] use a ratom, backed by a mutable source"
  [initial-val]
  (hooks/memo-once #(create initial-val)))

