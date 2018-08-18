(ns chia.jss.functional-styles
  (:require [chia.util :as u]
            [chia.jss :as jss]
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

(defn base [& [{:as options
                :keys [unit
                       break-small]
                :or {unit 8
                     break-small 420}}]]
  {"@global"
   (merge {:.flex {:display "flex"}
           :.flex-auto {:flex "1 1 auto"
                        :min-width 0
                        :min-height 0}
           :.justify-center {:justify-content "center"}
           :.flex-wrap {:flex-wrap "wrap"}
           :.items-center {:align-items "center"}
           :.center {:margin-left "auto"
                     :margin-right "auto"}
           :.text-center {:text-align "center"}
           :.hover-opacity-1 {"&:hover" {:opacity 1}}
           :.hover-opacity-parent {:cursor "pointer"
                                   "& .hover-opacity-child" {:opacity 0.5}
                                   "&:hover .hover-opacity-child" {:opacity 1}}
           :.overflow-hidden {:overflow "hidden"}
           :.b-solid {:border-style "solid"}
           :.b-none {:border "none"}
           :.no-underline {:text-decoration "none"}
           :.display-link {:text-decoration "none"
                           "&:hover" {:text-decoration "underline"}}}
          (u/for-map [side [:top :left :right :bottom]]
            {(str ".b-" (name side)) {(str "border-" (name side) "-style") "solid"}
             (str "." (name side) "-0") {side 0}})
          (u/for-map [[i v] (->> [unit
                                  (* 1.5 unit)
                                  (* 2 unit)
                                  (* 2.5 unit)
                                  (* 3 unit)
                                  (* 4 unit)]
                                 (interleave (range 1 999))
                                 (partition 2))]
            {(str ".height-" i) {:height v}
             (str ".width-" i) {:width v}})
          {:.width-100p {:width "100%"}}
          (u/for-map [n (range 17)]
            {(str ".round-" n) {:border-radius n}
             (str ".b-" n) {:border-width n}})
          (u/for-map [x ["pointer"
                         "default"]]
            {(str ".cursor-" x) {:cursor x}})
          (u/for-map [n (range 0 11)]
            {(str ".opacity-" n) {:opacity (* n 0.1)}})
          (u/for-map [[n v] (->> (interleave (range 1 5)
                                             [0.03 0.05 0.075 0.1])
                                 (partition 2))
                      :let [color (str "rgba(0,0,0," v ")")]]
            {(str ".b-darken-" n) {:border-color color}
             (str ".bg-darken-" n) {:background-color color}
             (str ".hover-bg-darken-" n) {"&:hover" {:background-color color}}})
          (u/for-map [[n v] {:orange "#fa7921"
                             :light-orange "#fe9920"
                             :green "#2e933c"
                             :dark-green "#306b34"
                             :dark-blue "#0c4767"
                             :white "#ffffff"
                             :black "#000000"}
                      :let [color-name (name n)]]
            {(str "." color-name) {:color v}
             (str ".bg-" color-name) {:background-color v}})
          (u/for-map [i (range 11)]
            {(str ".opacity-" (* i 10)) {:opacity (/ i 10)}})


          (with-breakpoint break-small
            (u/for-map [[rule values] {:display ["flex"
                                                 "block"
                                                 "inline-block"
                                                 "inline-flex"]
                                       :position ["relative"
                                                  "absolute"
                                                  "fixed"]}
                        value values]
              {(str "." value) {rule value}})
            (u/for-map [direction ["row"
                                   "column"]]
              {(str ".flex-" direction) {:flex-direction direction}})
            (u/for-map [n (conj (range 0 11)
                                0.5 1.5 2.5)]
              {(str ".margin-" (num->str n)) {:margin (* unit n)}
               (str ".margin-h-" (num->str n)) {:margin-left (* unit n)
                                                :margin-right (* unit n)}
               (str ".margin-v-" (num->str n)) {:margin-top (* unit n)
                                                :margin-bottom (* unit n)}
               (str ".margin-t-" (num->str n)) {:margin-top (* unit n)}
               (str ".margin-b-" (num->str n)) {:margin-bottom (* unit n)}
               (str ".pad-" (num->str n)) {:padding (* unit n)}
               (str ".pad-h-" (num->str n)) {:padding-left (* unit n)
                                             :padding-right (* unit n)}
               (str ".pad-v-" (num->str n)) {:padding-top (* unit n)
                                             :padding-bottom (* unit n)}
               (str ".pad-t-" (num->str n)) {:padding-top (* unit n)}
               (str ".pad-b-" (num->str n)) {:padding-bottom (* unit n)}
               })
            (for [px (range 8 30)]
              [(str ".text-size-" px) {:font-size (str px "px")}])))})