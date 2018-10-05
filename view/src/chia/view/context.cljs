(ns chia.view.context
  (:require ["react" :as react]
            [chia.view :as v]
            [chia.view.hiccup :as hiccup]
            [clojure.core :as core]
            [goog.object :as gobj])
  (:require-macros [chia.view.context]))

(aset js/window "React" react)

(defonce lookup-context
         (memoize
          (fn ^js [k]
            (if (keyword? k)
              (react/createContext (munge (str k)))
              k))))

(defn provide
  "Adds React contexts to the component tree.
   `bindings` should be a map of {<keyword-or-Context>, <value-to-be-bound>}."
  [binding-map & body]
  (loop [bindings (seq binding-map)
         out (v/to-element (vec (cons :<> body)))]
    (if (empty? bindings)
      out
      (recur (rest bindings)
             (let [[context-k context-v] (first bindings)]
               (-> (lookup-context context-k)
                   (.-Provider)
                   (react/createElement #js {:value context-v} out)))))))

(v/defview context-observer
  {:view/should-update (fn [{:keys [view/props
                                    view/prev-props
                                    view/state
                                    view/prev-state]}]
                         (or (not= (dissoc props :view-fn)
                                   (dissoc prev-props :view-fn))
                             (not= @state prev-state)))}
  [{:keys [view-fn
           context-value]}]
  (view-fn context-value))

(defn consume*
  "Reads a React context value within component tree.

   `context` should be a keyword or React Context instance."
  [^js ctx f]
  (react/createElement (.-Consumer ctx) #js {} #(context-observer {:view-fn f
                                                                   :context-value %})))


(comment
 (:require [web3.view.context :as c])

 (defn example []
   [c/provide {::first-name "Herman"}
    [c/consume ::first-name
     (fn [first-name]
       [:div
        (str "Hello, " @first-name)])]])

 [c/provide {::first-name (r/atom "Herman")
             my-theme #js {:color "pink"}}
  [c/consume ::first-name
   (fn [first-name]
     [c/consume my-theme
      (fn [theme]
        [:div
         {:style {:color (.-color theme)}
          :on-click #(swap! first-name str "+")}
         (str "Hello, " @first-name)])])]]
 [c/provide {:a 10
             :b 20}
  [c/provide {:a 11}
   [c/consume :a
    (fn [a]
      (let [{:keys [state] :as cthis} (r/current-component)]
        [:div
         (str "a: " a)
         [:div
          [m/button {:on-click #(swap! state assoc :button (rand-int 1000))} "c-state"]
          (:button @state)]
         "counter: " @counter
         [:br]

         [:br]]))]]])