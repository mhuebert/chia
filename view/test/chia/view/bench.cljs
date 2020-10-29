(ns chia.view.bench
  (:require
    ["benchmark" :as b]
    ["react" :as react :refer [Fragment] :rename {createElement rce}]
    ["react-dom/server" :as rdom]
    [reagent.core :as reagent]
    [cljs.test :as t]
    [chia.view :as v]
    [chia.view.class :as class]
    [chia.view.hiccup :as hiccup]
    [triple.view :as triple]
    [triple.view.hiccup :as triple-hiccup]
    [clojure.string :as str]
    [hicada.view :as hv])
  (:require-macros [chia.view.bench :as bench]
                   [hicada.compiler :as hc]
                   [hicada.infer :as infer]))

(def element react/createElement)
(def to-string rdom/renderToString)

(defn v0 []
  [:div "Hello v0"])

(hv/defview v1 [^number a ^number b]
  [:div "Hello v1" a b ^:hiccup [v0]])

(hv/defview v2 []
  [:div "Hello v2." [v1 1 2]])

(hv/defview hv-render [{:keys [title body items]}]
  [:div.card
   [:div.card-title ^string title]
   [:div.card-body ^string body]
   [:ul.card-list
    (for [^js item items]
      ^{:key item} [:li item])]
   [:div.card-footer
    [:div.card-actions
     [:button "ok"]
     [:button "cancel"]]]])

(comment
  (hv/defview hv [] [:div.card])
  (macroexpand '(hc/to-element [:div.card [:div.what title]])))

(def sample-props {:style {:font-size 10}
                   :class "red"})

(defn reagent-render [{:keys [title body items]}]
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
       [:button "cancel"]]]]))

(defn react-render [{:keys [title body items]}]

  (element "div" #js {:className "card"}
           (element "div" #js {:className "card-title"} title)
           (element "div" #js {:className "card-body"} body)
           (element "ul" #js {:className "card-list"}
                    (.apply element
                            nil
                            (reduce (fn [out item]
                                      (doto out (.push (element "li" #js {} item))))
                                    #js[Fragment nil] items)))
           (element "div" #js {:className "card-footer"}
                    (element "div" #js {:className "card-actions"}
                             (element "button" nil "ok")
                             (element "button" nil "cancel")))))

(v/defview chia-view [{:keys [title body items]}]
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

(triple/defview triple-view [{:keys [title body items]}]
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

(class/defclass chia-legacy [{:keys [title body items]}]
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

(defn chia-hiccup [{:keys [title body items]}]
  (hiccup/to-element
    [:div.card
     [:div.card-title title]
     [:div.card-body body]
     [:ul.card-list
      (for [item items]
        [:li {:key item} item])]
     [:div.card-footer
      [:div.card-actions
       [:button "ok"]
       [:button "cancel"]]]]))

(defn log-cycle [event]
  (println (.toString (.-target event))))

(defn sample-data []
  {:title "hello world"
   :body "body"
   :items (shuffle (range 10))})

(defn ^:dev/after-load main [& args]
  (let [test-data (sample-data)
        suite (b/Suite.)]
    (aset js/window "Benchmark" suite)
    #_(println "chia")
    #_(println (chia-view test-data))
    #_(println "react")
    #_(println (react-render test-data))
    #_(println "reagent")
    #_(println (reagent-render test-data))
    #_(println "hx")
    #_(println (hx-render test-data))
    #_(println "rum")
    #_(println (rum-render test-data))
    ;(js/console.profile "chia")
    ;(dotimes [n 10000] (to-string (chia-view test-data)))
    ;(js/console.profileEnd)


    (print :react "\n" (to-string (react-render test-data)))
    (print :chia-legacy "\n" (to-string (chia-legacy test-data)))
    (print :triple "\n" (to-string (triple-hiccup/to-element [triple-view test-data])))
    (print :hicada-view "\n" (to-string (hicada.view/to-element [hv-render test-data])))

    (-> suite

        (.add "reagent/interpret" (comp to-string
                                        #(reagent-render test-data)))
        (.add "chia-hiccup/interpret" (comp to-string
                                            #(chia-hiccup test-data)))
        (.add "triple/interpret" (comp to-string
                                       #(triple-hiccup/to-element
                                          [triple-view test-data])))

        (.add "hicada/view" (comp to-string
                                  #(hv/to-element
                                     [hv-render test-data])))

        (.on "cycle" log-cycle)
        (.run))))


(defonce _ (main))
