(ns yawn.emit-test
  (:require [yawn.compiler :as c])
  #?(:cljs (:require-macros [yawn.emit-test :refer [emit]])))

(defmacro emit [label expr]
  `(do (~'js/console.log "START" ~label "------------------------")
       (def ^:export ~(gensym (str "a" label)) ~expr)
       (~'js/console.log "END  " ~label "------------------------")))

#?(:cljs
   (do
     (emit 1 (c/as-element [:div]))

     (emit 2 (c/as-element [:div.a {:class "b"}]))

     (let [B "b"
           D "d"]
       (emit 3 (c/as-element [:div.a {:class [B "c" D]}])))

     (emit 4 (c/as-element [:div {:style {:font-size 12}}]))

     ))

