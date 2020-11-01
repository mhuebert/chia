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
    [hicada.view :as hv]
    [hicada.runtime :as hr])
  (:require-macros [chia.view.bench :as bench]
                   [hicada.compiler :as hc]
                   [hicada.infer :as infer]))

(def element react/createElement)
(def to-string rdom/renderToString)

(defn v0 []
  [:div "Hello v0"])

(hv/defview v1 [^number a ^number b]
  [:div "Hello v1" a b ^:interpret [v0]])

(hv/defview v2 []
  [:div "Hello v2." [v1 1 2]])

(defn plain-fn [{:keys [title body items]}]
  [:div.card
   {:class title}
   [:div.card-title {:style {:font-size 10
                             :height 20
                             :width 30}} title]
   [:div.card-body {:class [title body]} body]
   [:ul.card-list
    (for [item items]
      ^{:key item} [:li item])]
   [:div.card-footer
    [:div.card-actions
     [:button "ok"]
     [:button "cancel"]]]])

(def sample-props {:style {:font-size 10}
                   :class "red"})

(defn reagent-interpret [data]
  (reagent/as-element [plain-fn data]))

(defn react-render [{:keys [title body items]}]
  (hc/create-element "div" #js {:className (str "card " title)}
           (hc/create-element "div" #js {:className "card-title" :fontSize 10 :height 20 :width 30} title)
           (hc/create-element "div" #js {:className (str "card-body "
                                                         title " "
                                                         body)} body)
           (hc/create-element "ul" #js {:className "card-list"}
                    (.apply hr/createElement
                            nil
                            (reduce (fn [out item]
                                      (doto out (.push (element "li" #js {} item))))
                                    #js[hr/Fragment nil] items)))
           (hc/create-element "div" #js {:className "card-footer"}
                    (hc/create-element "div" #js {:className "card-actions"}
                             (hc/create-element "button" nil "ok")
                             (hc/create-element "button" nil "cancel")))))

(v/defview chia-view [{:keys [title body items]}]
  [:div.card
   {:class title}
   [:div.card-title {:style {:font-size 10
                             :height 20
                             :width 30}} title]
   [:div.card-body {:class [title body]} body]
   [:ul.card-list
    (for [item items]
      ^{:key item} [:li item])]
   [:div.card-footer
    [:div.card-actions
     [:button "ok"]
     [:button "cancel"]]]])

(triple/defview triple-view [{:keys [title body items]}]
  [:div.card
   {:class title}
   [:div.card-title {:style {:font-size 10
                             :height 20
                             :width 30}} title]
   [:div.card-body {:class [title body]} body]
   [:ul.card-list
    (for [item items]
      ^{:key item} [:li item])]
   [:div.card-footer
    [:div.card-actions
     [:button "ok"]
     [:button "cancel"]]]])

(class/defclass chia-legacy [{:keys [title body items]}]
  [:div.card
   {:class title}
   [:div.card-title {:style {:font-size 10
                             :height 20
                             :width 30}} title]
   [:div.card-body {:class [title body]} body]
   [:ul.card-list
    (for [item items]
      ^{:key item} [:li item])]
   [:div.card-footer
    [:div.card-actions
     [:button "ok"]
     [:button "cancel"]]]])

(defn chia-hiccup [{:keys [title body items]}]
  (hiccup/to-element
    [:div.card
     {:class title}
     [:div.card-title {:style {:font-size 10
                               :height 20
                               :width 30}} title]
     [:div.card-body {:class [title body]} body]
     [:ul.card-list
      (for [item items]
        ^{:key item} [:li item])]
     [:div.card-footer
      [:div.card-actions
       [:button "ok"]
       [:button "cancel"]]]]))

(defn log-cycle [event]
  (println (.toString (.-target event))))

(defn sample-data []
  {:title "the-title"
   :body "the-body"
   :items (shuffle (range 10))})

(hv/defview hicada-view [{:keys [^string title ^string body items]}]
  [:div.card
   {:class title}
   [:div.card-title {:style {:font-size 10
                             :height 20
                             :width 30}} title]
   [:div.card-body {:class [title body]} body]
   [:ul.card-list
    (for [^js item items]
      ^{:key item} [:li item])]
   [:div.card-footer
    [:div.card-actions
     [:button "ok"]
     [:button "cancel"]]]])

(defn hicada-element [{:keys [^js title ^js body items]}]
  (hc/to-element
    [:div.card
     {:class title}
     [:div.card-title {:style {:font-size 10
                               :height 20
                               :width 30}} title]
     [:div.card-body {:class [title body]} body]
     [:ul.card-list
      (for [^js item items]
        ^{:key item} [:li item])]
     [:div.card-footer
      [:div.card-actions
       [:button "ok"]
       [:button "cancel"]]]]))

(hv/defview hv-interpret [data]
  [plain-fn data])

(comment
  (hv/defview hv [] [:div.card])
  (macroexpand '(hc/to-element [:div.card [:div.what title]])))

(defn ^:dev/after-load main [& args]
  (let [test-data (sample-data)
        suite (b/Suite.)]
    (aset js/window "Benchmark" suite)
    #_(println "chia")
    #_(println (chia-view test-data))
    #_(println "react")
    #_(println (react-render test-data))
    #_(println "reagent")
    #_(println (reagent-interpret test-data))
    #_(println "hx")
    #_(println (hx-render test-data))
    #_(println "rum")
    #_(println (rum-render test-data))
    ;(js/console.profile "chia")
    ;(dotimes [n 10000] (to-string (chia-view test-data)))
    ;(js/console.profileEnd)


    (print :reagent "\n" (to-string (reagent-interpret test-data)))
    ;(print :chia-legacy "\n" (to-string (chia-legacy test-data)))
    ;(print :triple "\n" (to-string (triple-hiccup/to-element [triple-view test-data])))
    (print :hicada/view "\n" (to-string (hicada-view test-data)))
    (print :hicada/interpret "\n" (to-string (hr/to-element [plain-fn test-data])))
    ;(print :react "\n" (to-string (react-render test-data)))


    (-> suite

        (.add "reagent/interpret" (comp to-string
                                        #(reagent-interpret test-data)))
        (.add "chia-hiccup/interpret" (comp to-string
                                              #(chia-hiccup test-data)))
        (.add "triple/interpret" (comp to-string
                                         #(triple-hiccup/to-element
                                            [triple-view test-data])))

        (.add "react" (comp to-string #(react-render test-data)))
        (.add "hicada/view" (comp to-string #(hicada-view test-data)))
        (.add "hicada/element" (comp to-string #(hicada-element test-data)))
        (.add "hicada/interpret" (comp to-string #(hr/to-element [plain-fn test-data])))


        (.on "cycle" log-cycle)
        (.run))))


(defonce _ (do (main)
               (.addEventListener js/window "click" #(let [test-data (sample-data)]
                                                       (dotimes [n 10000]
                                                         (to-string (hr/to-element [plain-fn test-data])))))))
