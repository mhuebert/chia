(ns chia.view.bench
  (:require
    [applied-science.js-interop :as j]
    ["benchmark" :as b]
    ["react" :as react :refer [Fragment] :rename {createElement rce}]
    ["react-dom/server" :as rdom]
    [reagent.core :as reagent]
    [cljs.test :as t]
    [clojure.string :as str]
    [hicada.view :as hv]
    [hicada.convert :as hr]
    [hicada.env :as henv]
    [test.hicada.macros-test :as hm]
    [applied-science.js-interop :as j])
  (:require-macros [hicada.macros :as m]
                   [chia.view.bench :as bench]
                   [hicada.compiler :as hc]
                   [hicada.infer :as infer]))

(def element react/createElement)
(def to-string rdom/renderToStaticMarkup)

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
  (hv/as-element
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

(def sample-props {:style {:font-size 10}
                   :class "red"})

(defn reagent-interpret [data]
  (reagent/as-element [plain-fn data]))

(defn react-render [{:keys [title body items]}]
  (react/createElement "div" #js {:className (str "card " title)}
                       (react/createElement "div" #js {:style #js{:fontSize 10 :height 20 :width 30}
                                                       :className "card-title"} title)
                       (react/createElement "div" #js {:className (str "card-body "
                                                                       title " "
                                                                       body)} body)
                       (react/createElement "ul" #js {:className "card-list"}
                                            (.apply react/createElement
                                                    nil
                                                    (reduce (fn [out item]
                                                              (doto out (.push (element "li" #js {} item))))
                                                            #js[react/Fragment nil] items)))
                       (react/createElement "div" #js {:className "card-footer"}
                                            (react/createElement "div" #js {:className "card-actions"}
                                                                 (react/createElement "button" nil "ok")
                                                                 (react/createElement "button" nil "cancel")))))

(defn log-cycle [event]
  (println (.toString (.-target event))))

(defn sample-data []
  {:title "the-title"
   :body "the-body"
   :items (shuffle (range 10))})

(hv/defview hv-interpret [data]
  [plain-fn data])

(comment
  (hv/defview hv [] [:div.card])
  (macroexpand '(hc/to-element [:div.card [:div.what title]])))

(defn main [& args]
  (let [test-data (sample-data)
        suite (b/Suite.)]
    (aset js/window "Benchmark" suite)
    (print :reagent "\n" (to-string (reagent-interpret test-data)))
    (print :hicada/view "\n" (to-string (hicada-view test-data)))
    (print :hicada/interpret "\n" (to-string (hv/as-element [plain-fn test-data])))
    (print :react "\n" (to-string (react-render test-data)))


    (-> suite


        (.add "hicada/view" (comp to-string #(hicada-view test-data)))
        (.add "hicada/element" (comp to-string #(hicada-element test-data)))
        (.add "hicada/interpret" (comp to-string #(hv/as-element [plain-fn test-data])))
        (.add "reagent/interpret" (comp to-string #(reagent-interpret test-data)))
        (.add "react" (comp to-string #(react-render test-data)))


        (.on "cycle" log-cycle)
        (.run))))

(let [A "a"
      B "b"]
  (assert (=
            (hm/join-strings-macro ["a" "b"])
            (hm/join-strings-macro [A B]))
          "util/casetime, equivalent results")
  (assert (=
            '(clojure.string/join [A B])
            (macroexpand '(hm/join-strings-macro [A B])))

          "util/casetime, deftime expanding to form")
  (assert (= "ab"
             (macroexpand '(hm/join-strings-macro ["a" "b"])))
          "util/casetime, deftime expanding to result"))

#_(defonce _ (main))
