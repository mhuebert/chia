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
   (merge {:.hover-opacity-1 {"&:hover" {:opacity 1}}
           :.hover-opacity-parent {:cursor "pointer"
                                   "& .hover-opacity-child" {:opacity 0.5}
                                   "&:hover .hover-opacity-child" {:opacity 1}}
           #_#_:.overflow-hidden {:overflow "hidden"}
           :.b-solid {:border-style "solid"}
           :.b-none {:border "none"}
           :.no-underline {:text-decoration "none"}
           :.display-link {:text-decoration "none"
                           :cursor "pointer"
                           "&:hover" {:text-decoration "underline"}}}
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
          (u/for-map [side [:top :left :right :bottom]]
              {(str ".b-" (name side)) {(str "border-" (name side) "-style") "solid"}
               (str "." (name side) "-0") {side 0}})
          #_(u/for-map [[i v] (->> [unit
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
          #_(u/for-map [x ["pointer"
                           "default"]]
              {(str ".cursor-" x) {:cursor x}})
          (u/for-map [n (range 0 11)]
            {(str ".opacity-" n) {:opacity (* n 0.1)}})
          (u/for-map [[n v] (->> (interleave (range 1 9)
                                             [0.015 0.03 0.05 0.075 0.1 0.25 0.5 0.75])
                                 (partition 2))
                      :let [color (str "rgba(0,0,0," v ")")]]
            {(str ".b-darken-" n) {:border-color color}
             (str ".bg-darken-" n) {:background-color color}
             (str ".darken-" n) {:color color}
             (str ".hover-bg-darken-" n) {"&:hover" {:background-color color}}})
          (u/for-map [[n v] {:orange "#fa7921"
                             :light-orange "#fe9920"
                             :green "#2e933c"
                             :dark-green "#306b34"
                             :dark-blue "#0c4767"
                             :white "#ffffff"
                             :black "#000000"
                             :error-pink "#faeaed"}
                      :let [color-name (name n)]]
            {(str "." color-name) {:color v}
             (str ".bg-" color-name) {:background-color v}})
          #_(u/for-map [i (range 11)]
              {(str ".opacity-" (* i 10)) {:opacity (/ i 10)}})


          (with-breakpoint break-small
            {#_#_:.flex {:display "flex"}
             #_#_:.flex-auto {:flex "1 1 auto"
                              :min-width 0
                              :min-height 0}
             #_#_:.flex-none {:flex "none"}
             #_#_:.justify-center {:justify-content "center"}
             #_#_:.flex-wrap {:flex-wrap "wrap"}
             #_#_:.items-center {:align-items "center"}
             #_#_:.items-stretch {:align-items "stretch"}
             #_#_:.items-end {:align-items "flex-end"}
             #_#_:.center {:margin-left "auto"
                           :margin-right "auto"}
             :.text-center {:text-align "center"}}
            (u/for-map [[rule values] {:display ["flex"
                                                   "block"
                                                   "inline-block"
                                                   "inline-flex"]
                                         :position ["relative"
                                                    "absolute"
                                                    "fixed"]}
                          value values]
                {(str "." value) {rule value}})
            #_(u/for-map [direction ["row"
                                     "column"]]
                {(str ".flex-" direction) {:flex-direction direction}})
            #_(u/for-map [n (conj (range 0 11)
                                  0.5 1.5 2.5)]
                {(str ".margin-" (num->str n)) {:margin (* unit n)}
                 (str ".margin-h-" (num->str n)) {:margin-left (* unit n)
                                                  :margin-right (* unit n)}
                 (str ".margin-v-" (num->str n)) {:margin-top (* unit n)
                                                  :margin-bottom (* unit n)}
                 (str ".margin-t-" (num->str n)) {:margin-top (* unit n)}
                 (str ".margin-b-" (num->str n)) {:margin-bottom (* unit n)}
                 (str ".margin-r-" (num->str n)) {:margin-right (* unit n)}
                 (str ".margin-l-" (num->str n)) {:margin-left (* unit n)}

                 (str ".margin-t-n" (num->str n)) {:margin-top (* unit (- n))}
                 (str ".margin-b-n" (num->str n)) {:margin-bottom (* unit (- n))}
                 (str ".margin-r-n" (num->str n)) {:margin-right (* unit (- n))}
                 (str ".margin-l-n" (num->str n)) {:margin-left (* unit (- n))}
                 (str ".margin-h-n" (num->str n)) {:margin-left (* unit (- n))
                                                   :margin-right (* unit (- n))}
                 (str ".margin-v-n" (num->str n)) {:margin-top (* unit (- n))
                                                   :margin-bottom (* unit (- n))}

                 (str ".pad-" (num->str n)) {:padding (* unit n)}
                 (str ".pad-h-" (num->str n)) {:padding-left (* unit n)
                                               :padding-right (* unit n)}
                 (str ".pad-v-" (num->str n)) {:padding-top (* unit n)
                                               :padding-bottom (* unit n)}
                 (str ".pad-t-" (num->str n)) {:padding-top (* unit n)}
                 (str ".pad-b-" (num->str n)) {:padding-bottom (* unit n)}
                 })
            (for [px (range 8 30)]
              [(str ".text-size-" px) {:font-size (str px "px")}])
            (for [n [18 24 36 48]]
              [(str ".md-" n) {:font-size n}])
            ;.material-icons.md-18 { font-size: 18px; }
            ;.material-icons.md-24 { font-size: 24px; }
            ;.material-icons.md-36 { font-size: 36px; }
            ;.material-icons.md-48 { font-size: 48px; }
            ))})