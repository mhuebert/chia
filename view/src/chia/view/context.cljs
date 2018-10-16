(ns chia.view.context
  (:require ["react" :as react]
            [chia.view :as v]
            [chia.view.hiccup :as hiccup]
            [clojure.core :as core]
            [goog.object :as gobj]
            [chia.util.js-interop :as j])
  (:require-macros [chia.view.context]))

(j/assoc! js/window :React react)

(def create-element react/createElement)
(def create-context react/createContext)

(defonce lookup-context
         (memoize
          (fn ^js [k]
            (if (keyword? k)
              (create-context (munge (str k)))
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
                   (j/get :Provider)
                   (create-element #js {:value context-v} out)))))))

(v/defview context-observer
  {:view/should-update (constantly true)}
  [{:keys [view-fn
           context-value]}]
  (view-fn context-value))

(defn consume*
  "Reads a React context value within component tree.

   `context` should be a keyword or React Context instance."
  [^js ctx f]
  (-> ctx
      (j/get :Consumer)
      (create-element #js {} #(context-observer {:view-fn f
                                                 :context-value %}))))


(comment

 (defn example []
   (provide {::first-name "Herman"}
     (c/consume [first-name ::first-name]
                [:div
                 (str "Hello, " @first-name)])))

 (provide {::first-name (r/atom "Herman")
           my-theme #js {:color "pink"}}
   (c/consume [first-name ::first-name
               theme my-theme]
              [:div
               {:style {:color (.-color theme)}
                :on-click #(swap! first-name str "+")}
               (str "Hello, " @first-name)]))
 (provide {:a 10
           :b 20}
   (provide {:a 11}
     (c/consume [a :a]
                (let [{:keys [state] :as cthis} (r/current-component)]
                  [:div
                   (str "a: " a)
                   [:div
                    [m/button {:on-click #(swap! state assoc :button (rand-int 1000))} "c-state"]
                    (:button @state)]
                   "counter: " @counter
                   [:br]

                   [:br]])))))