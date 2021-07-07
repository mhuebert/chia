(ns yawn.emit-test
  (:require [applied-science.js-interop :as j]
            [yawn.compiler :as c]
            [clojure.string :as str])
  #?(:cljs (:require-macros [yawn.emit-test :refer [emit]])))

(defmacro emit [label expr]
  (let [name (gensym (str "a" label))]
    `(do (~'js/console.log "START" ~label "------------------------")
         (def ^:export ~name ~expr)
         (~'js/console.log ~name)
          (~'js/console.log "END  " ~label "------------------------"))))

  #?(:cljs
   (do
     (emit 1 (c/as-element [:div]))

     (emit 2 (c/as-element [:div.a {:class "b"}]))

     (let [B "b"
           D "d"]
       (emit 3 (c/as-element [:div.a {:class [B "c" D]}])))

     (emit 4 (c/as-element [:div {:style {:font-size 12}}]))

     ;; precompiles via closure
     (let [b "b"]
       (emit 5 (str "a" " " b " " "c")))

     ;; does not precompile
     (let [b "b"]
       (emit 6 (str/join ["a" " " b " " "c"])))

     (emit 7 (js-obj "a" 1 "b" 2))

     ))

