(ns chia.jss.functional-styles
  (:require [chia.util :as u]
            [clojure.string :as str]))

(defn num->str [n]
  (-> (str n)
      (str/replace "." "_")))

(defn append-to-name [s postfix]
  (str (name s) postfix))

(defn with-breakpoint [breakpoint & ms]
  (let [m (apply merge ms)]
    (merge m
           {(str "@media (max-width: " breakpoint "px)")
            (u/update-keys #(append-to-name % "-sm") m)}
           {(str "@media (min-width: " (inc breakpoint) "px)")
            (u/update-keys #(append-to-name % "-lg") m)})))

(defn color-styles [color-map]
  (u/for-map [[n v] color-map
              :let [color-name (name n)]]
    {(str "." color-name) {:color v}
     (str ".bg-" color-name) {:background-color v}
     (str ".b--" color-name) {:border-color v}}))

(defn base [& [{:as options
                :keys [unit
                       break-small]
                :or {unit 8
                     break-small 420}}]]
  {"@global"
   (merge {:.hover-opacity-1 {"&:hover" {:opacity 1}}
           :.hover-opacity-parent {:cursor "pointer"
                                   "& .hover-opacity-child" {:opacity 0.5}
                                   "&:hover .hover-opacity-child" {:opacity 1}}
           #_#_:.overflow-hidden {:overflow "hidden"}
           :.b--solid {:border-style "solid"}
           :.b--none {:border "none"}
           :.no-underline {:text-decoration "none"}
           :.display-link {:text-decoration "none"
                           :cursor "pointer"
                           "&:hover" {:text-decoration "underline"}}
           :.pre-wrap {:whitespace "pre-wrap"}}
          (u/for-map [[key weight] (seq {"thin" 100
                                         "extra-light" 200
                                         "light" 300
                                         "normal" 400
                                         "medium" 500
                                         "semi-bold" 600
                                         "bold" 700
                                         "extra-bold" 800
                                         "heavy" 900})]
            {(str ".weight-" key) {:font-weight weight}})
          {:.width-100p {:width "100%"}}
          (u/for-map [n (range 17)]
            {(str ".round-" n) {:border-radius n}
             (str ".b--" n) {:border-width n}})
          (u/for-map [n (range 0 11)]
            {(str ".opacity-" n) {:opacity (* n 0.1)}})
          (u/for-map [[n v] (->> (interleave (range 1 9)
                                             [0.015 0.03 0.05 0.075 0.1 0.25 0.5 0.75])
                                 (partition 2))
                      :let [color (str "rgba(0,0,0," v ")")]]
            {(str ".b--darken-" n) {:border-color color}
             (str ".bg-darken-" n) {:background-color color}
             (str ".darken-" n) {:color color}
             (str ".hover-bg-darken-" n) {"&:hover" {:background-color color}}})
          (with-breakpoint break-small
            (u/for-map [px (range 8 30)]
              {(str ".text-size-" px) {:font-size (str px "px")}})
            (u/for-map [n [18 24 36 48]]
              {(str ".md-" n) {:font-size n}})))})