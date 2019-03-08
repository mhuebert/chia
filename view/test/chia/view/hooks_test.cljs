(ns chia.view.hooks-test
  (:require [chia.view :as v]
            [chia.view.hooks :as hooks]
            [cljs.test :refer [deftest testing is are]]
            [chia.view.util :as u]
            [chia.triple-db :as d]
            [applied-science.js-interop :as j]
            [chia.view.registry :as registry]))

(defonce test-element (u/find-or-append-element (str ::element)))

(defn render! [x]
  (v/render-to-dom x test-element {:reload? false}))

(def render-count (atom 0))
(def internal-state-atom (atom nil))

(defn update-db! []
  (d/transact! [[:db/update-attr ::view-test :x inc]])
  (v/flush!))

(v/defn memoized-view [x]
  (d/get ::view-test :x)
  (swap! render-count inc)
  (str x))

(v/defn always-view
  {:view/should-update? (constantly true)}
  [x]
  (d/get ::view-test :x)
  (swap! render-count inc)
  (str x))

(v/defn view-with-state []
  (let [st (hooks/use-state)]
    (swap! render-count inc)
    (reset! internal-state-atom st)
    (str "state: " @st)))

(deftest hooks
  (testing "memoization"

      (reset! render-count 0)
      (render! (memoized-view 1))
      (render! (memoized-view 1))
      (is (= 1 @render-count)
        "View does not re-render with same args")
    (render! (memoized-view 2))
    (is (= 2 @render-count)
          "View re-renders with different args")
    (update-db!)
    (is (= 3 @render-count)
          "View re-renders on chia.view/reactive invalidation"))

  (testing "should-update?"

    (reset! render-count 0)
    (render! (always-view 1))
    (render! (always-view 1))
    (is (= 2 @render-count)
        "view/should-update? works")
    (update-db!)
    (is (= 3 @render-count)
        "view/should-update? does not interfere with chia.view/reactive invalidations"))

  (testing "hooks/use-state"
    (reset! render-count 0)
    (render! (view-with-state))
    (swap! @internal-state-atom inc)
    (v/flush!)
    (is (= 2 @render-count)
        "swapping internal state atom causes render"))

  (testing "ref forwarding"
    (def parent-ref (atom nil))
    (v/defn f-view
      {:view/forward-ref? true}
      []
      (let [ref (hooks/use-forwarded-ref)]
        [:div.black {:ref ref}]))

    (v/defn f-parent []
      (let [ref (hooks/use-ref 0)]
        (do (reset! parent-ref ref) nil)
        (d/get ::view-test :x)
        [f-view {:ref ref} "child"]))

    (render! (f-parent "parent"))
    (update-db!)

    (is (= (j/get-in @parent-ref [:current :tagName])
           "DIV")
        "Child ref is forwarded to parent"))

  )