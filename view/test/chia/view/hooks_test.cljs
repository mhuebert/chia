(ns chia.view.hooks-test
  (:require ["react" :as react]
            ["react-dom/test-utils" :as test-utils]
            [chia.view :as v]
            [chia.view.hooks :as hooks]
            [cljs.test :refer [deftest testing is are]]
            [chia.view.util :as u]
            [chia.db :as d]
            [applied-science.js-interop :as j]
            [chia.view.registry :as registry]))

(def concurrent? false)
(def multiplier (if concurrent? 2 1))

(defn act [f]
  (test-utils/act #(do (f) js/undefined)))

(defonce test-element (u/find-or-append-element (str ::element)))


(v/defn concurrent [x]
  (v/-create-element react/unstable_ConcurrentMode nil x))



(defn render! [x]
  (act (fn []
         (v/render-to-dom (cond-> x concurrent? (concurrent)) test-element {:reload? false})
         js/undefined)))

(def render-count (atom 0))

(defn unmount! []
  (reset! render-count 0)
  (v/unmount-from-dom test-element))

(def internal-state-atom (atom nil))

(defn update-db! []
  (d/transact! [[:db/update-attr ::view-test :x inc]])
  (act v/flush!)
  js/undefined)

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

(v/defn view-with-atom []
  (prn :view-with-atom-start)
  (let [st (hooks/use-atom)]
    (swap! render-count inc)
    (reset! internal-state-atom st)
    (str "state: " @st)))

(deftest hooks

  (testing "memoization"

    (unmount!)
    (render! (memoized-view 1))
    (is (= (* multiplier 1) @render-count) "First render")

    (render! (memoized-view 1))
    (is (= (* multiplier 1) @render-count)
        "View does not re-render with same args")
    (render! (memoized-view 2))
    (is (= (* multiplier 2) @render-count)
        "View re-renders with different args")

    (update-db!)

    (is (= (* 3 multiplier) @render-count)
        "View re-renders on chia.view/reactive invalidation"))

  (testing "should-update?"

    (unmount!)
    (render! (always-view 1))
    (render! (always-view 1))
    (is (= (* 2 multiplier) @render-count)
        "view/should-update? works")

    (update-db!)
    (is (= (* 3 multiplier) @render-count)
        "view/should-update? does not interfere with chia.view/reactive invalidations"))

  (testing "hooks/use-atom"
    (unmount!)
    (render! (view-with-atom))
    (swap! @internal-state-atom inc)
    (act v/flush!)
    (is (= (* 2 multiplier) @render-count)
        "swapping internal state atom causes render"))

  (testing "ref forwarding"
    (unmount!)
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