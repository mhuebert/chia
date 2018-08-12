(ns chia.jss.functional-styles
  (:require [chia.util :as u]
            [chia.jss :as jss]))

(defn base [{:as options
               :keys [unit]
               :or {unit 8}}]
  {"@global"
   (merge {:.flex {:display "flex"}
           :.flex-auto {:flex "1 1 auto"
                        :min-width 0
                        :min-height 0}
           :.justify-center {:Justify-content "center"}
           :.flex-wrap {:flex-wrap "wrap"}
           :.items-center {:align-items "center"}
           :.hover-opacity-1 {"&:hover" {:opacity 1}}
           :.hover-opacity-parent {:cursor "pointer"
                                   "& .hover-opacity-child" {:opacity 0.5}
                                   "&:hover .hover-opacity-child" {:opacity 1}}
           :.overflow-hidden {:overflow "hidden"}
           :.b-solid {:border-style "solid"}
           :.b-none {:border "none"}}
          (u/for-map [side [:top :left :right :bottom]]
            {(str ".b-" (name side)) {(str "b-" (name side) "-style") "solid"}})
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
          (u/for-map [n (range 17)]
            {(str ".round-" n) {:border-radius n}
             (str ".b-" n) {:border-width n}})
          (u/for-map [[rule values] {:display ["flex"
                                               "block"
                                               "inline-block"]
                                     :position ["relative"
                                                "absolute"
                                                "fixed"]}
                      value values]
            {(str "." value) {rule value}})
          (u/for-map [direction ["row"
                                 "column"]]
            {(str ".flex-" direction) {:flex-direction direction}})
          (u/for-map [x ["pointer"
                         "default"]]
            {(str ".cursor-" x) {:cursor x}})
          (u/for-map [n (range 0 11)]
            {(str ".margin-" n) {:margin (* unit n)}
             (str ".margin-h-" n) {:margin-left (* unit n)
                                   :margin-right (* unit n)}
             (str ".margin-v-" n) {:margin-top (* unit n)
                                   :margin-bottom (* unit n)}
             (str ".pad-" n) {:padding (* unit n)}
             (str ".pad-h-" n) {:padding-left (* unit n)
                                :padding-right (* unit n)}
             (str ".pad-v-" n) {:padding-top (* unit n)
                                :padding-bottom (* unit n)}})
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
            {(str ".opacity-" (* i 10)) {:opacity (/ i 10)}}))})