(ns chia.view.bench
  (:require
   ["benchmark" :as b]
   ["react" :as react :refer [Fragment] :rename {createElement rce}]
   ["react-dom/server" :as rdom]
   [hx.react :as hx]
   [reagent.core :as reagent]
   [rum.core :as rum]
   [fulcro.client.dom :as fulcro-dom]
   [cljs.test :as t]
   [chia.view :as v]
   [chia.view.hiccup :as hiccup]
   [sablono.interpreter :as sab]))

(defn reagent-render [{:keys [title body items]}]
  (rdom/renderToString
   (reagent/as-element
    [:div.card
     [:div.card-title title]
     [:div.card-body body]
     [:ul.card-list
      (for [item items]
        ^{:key item} [:li item])]
     [:div.card-footer
      [:div.card-actions
       [:button "ok"]
       [:button "cancel"]]]])))

#_(defn shadow-render [{:keys [title body items]}]
    (rdom/renderToString
     (shadow/<<
      [:div.card
       [:div.card-title title]
       [:div.card-body body]
       [:ul.card-list
        (shadow/render-seq
         items
         identity
         (fn [item]
           (shadow/<< [:li item])))]
       [:div.card-footer
        [:div.card-actions
         [:button "ok"]
         [:button "cancel"]]]])))

(defn react-render [{:keys [title body items]}]
  (rdom/renderToString
   (rce "div" #js {:className "card"}
        (rce "div" #js {:className "card-title"} title)
        (rce "div" #js {:className "card-body"} body)
        (rce "ul" #js {:className "card-list"}
             (.apply rce
                     nil
                     (reduce (fn [out item]
                               (doto out (.push (rce "li" #js {} item))))
                             #js[Fragment nil] items)))
        (rce "div" #js {:className "card-footer"}
             (rce "div" #js {:className "card-actions"}
                  (rce "button" nil "ok")
                  (rce "button" nil "cancel"))))))

(v/defn chia-view [{:keys [title body items]}]
  [:div {:data-reactroot "" :class "card"}
   [:div {:class "card-title"} title]
   [:div {:class "card-body"} body]
   [:ul {:class "card-list"}
    (for [item items]
      [:li {:key item} item])]
   [:div {:class "card-footer"}
    [:div {:class "card-actions"}
     [:button "ok"]
     [:button "cancel"]]]])

(defn chia-render [data]
  (rdom/renderToString (chia-view data)))

(defn hx-render [{:keys [title body items]}]
  (rdom/renderToString
   (hx/f
    [:div {:class "card"}
     [:div {:class "card-title"} title]
     [:div {:class "card-body"} body]
     [:ul {:class "card-list"}
      (for [item items]
        [:li {:key item} item])]
     [:div {:class "card-footer"}
      [:div {:class "card-actions"}
       [:button "ok"]
       [:button "cancel"]]]])))

(rum/defc rumc [{:keys [title body items]}]
          [:div.card
           [:div.card-title title]
           [:div.card-body body]
           [:ul.card-list
            (for [item items]
              [:li {:key item} item])]
           [:div.card-footer
            [:div.card-actions
             [:button "ok"]
             [:button "cancel"]]]])

(defn rum-render [props]
  (rdom/renderToString
   (rumc props)))

(defn sablono-interpret [{:keys [title body items]}]
  (rdom/renderToString
   (sab/interpret
    [:div.card
     [:div.card-title title]
     [:div.card-body body]
     [:ul.card-list
      (for [item items]
        [:li {:key item} item])]
     [:div.card-footer
      [:div.card-actions
       [:button "ok"]
       [:button "cancel"]]]])))

(defn log-cycle [event]
  (println (.toString (.-target event))))

(defn log-complete [event]
  (this-as this
    (js/console.log this)))

(defn ^:dev/after-load main [& args]
  (let [test-data {:title "hello world"
                   :body  "body"
                   :items (shuffle (range 10))}]
    (js/console.profile "chia")
    (simple-benchmark [] (chia-render test-data) 10000)
    (js/console.profileEnd))
  (let [test-data {:title "hello world"
                   :body  "body"
                   :items (shuffle (range 10))}
        suite (b/Suite.)]
    (aset js/window "Benchmark" suite)
    #_(println "chia")
    #_(println (chia-render test-data))
    #_(println "react")
    #_(println (react-render test-data))
    #_(println "reagent")
    #_(println (reagent-render test-data))
    #_(println "hx")
    #_(println (hx-render test-data))
    #_(println "rum")
    #_(println (rum-render test-data))

    (when-not (= (react-render test-data)
                 (reagent-render test-data)
                 (chia-render test-data)
                 #_(shadow-render test-data)
                 (hx-render test-data)
                 (rum-render test-data)
                 (sablono-interpret test-data))
      (throw (ex-info "not equal!" {})))

    (-> suite

        #_(.add "react" #(react-render test-data))
        #_(.add "reagent" #(reagent-render test-data))
        #_(.add "hx" #(hx-render test-data))
        #_(.add "rum" #(rum-render test-data))
        #_(.add "sablono-interpret" #(sablono-interpret test-data))

        (.add "chia" #(chia-render test-data))

        #_(.add "shadow" #(shadow-render test-data))
        #_(.add "fulcro-dom" #(fulcro-dom-render test-data))
        (.on "cycle" log-cycle)
        ;; (.on "complete" log-complete)
        (.run))))



(defonce _ (main))


