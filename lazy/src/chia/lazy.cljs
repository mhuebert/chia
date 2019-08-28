(ns chia.lazy
  (:refer-clojure :exclude [some resolve])
  (:require [shadow.lazy :as lazy]
            [kitchen-async.promise :as p]
            [applied-science.js-interop :as j]
            [chia.util :as u])
  (:require-macros chia.lazy)
  (:import [goog.module ModuleManager ModuleLoader]))

(defn lazy? [x] (satisfies? lazy/ILoadable x))

(defprotocol ILoad
  (load [x]))

(extend-type lazy/Loadable
  ILoad
  (load [x] (lazy/load x)))

(deftype LoadableLookup [loadable lookup]
  lazy/ILoadable
  (ready? [_] (lazy/ready? loadable))
  ILoad
  (load [_] (load loadable))
  IDeref
  (-deref [_] (lookup @loadable)))

(defn- load-loadable [x]
  (if (lazy? x)
    (lazy/load x)
    x))

(defprotocol IAsyncStatus
  (loaded? [x])
  (error [x])
  (resolve! [x val])
  (error! [x err]))

(deftype CheckedPromise [x state]
  Object
  (then [this f]
    (cond (loaded? this) (f @this)
          (and (lazy? x) (lazy/ready? x)) (f @(resolve! this @x))
          :else (p/then (load-loadable x) f)))
  (then [this f g]
    (cond (loaded? this) (f @this)
          (and (lazy? x) (lazy/ready? x)) (f @(resolve! this @x))
          (error this) (g (error this))
          :else (p/then (load-loadable x) f g)))
  (catch [this f] (if-let [e (error this)]
                    (f e)
                    (p/catch* x f)))
  IDeref
  (-deref [this]
    (if (loaded? this)
      (j/get state :value)
      (throw (ex-info "Promise not ready" {:promise x}))))
  IAsyncStatus
  (loaded? [_] (false? (j/get state :loading)))
  (error [_] (j/get state :error))
  (resolve! [this val]
    (j/assoc! state :value val :loading false)
    this)
  (error! [_ e]
    (j/assoc! state :error e :loading false)))

(defn checked* [x]
  (let [a (j/obj :loading true)
        checked-promise (->CheckedPromise x a)]
    (p/try (p/let [val (load-loadable x)]
             (resolve! checked-promise val))
           (p/catch :default e
             (j/assoc! x :loading false :error e)))
    checked-promise))

(defn checked [x]
  (u/memoized-on x :lazyCheckedPromise (checked* x)))

