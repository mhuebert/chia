(ns yawn.compiler-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :as t :refer [deftest is are]]
            [yawn.convert :as convert]
            [yawn.compiler :as compiler :refer [compile-props]]
            [yawn.env :as env]
            [yawn.infer :as infer]
            [yawn.view :refer [as-element]]
            #?@(:cljs
                [[yawn.selfhost-test :as selfhost
                  :refer [eval-sync]
                  :rename {eval-sync e}]
                 ["react" :as react :refer [Fragment] :rename {createElement rce}]
                 ["react-dom/server" :as rdom]]))
  #?(:cljs (:require-macros [yawn.compiler-test :refer [--]])))

(env/def-options no-interpret {:throw-on-interpretation? :throw})

(defmacro -- [doc & pairs]
  (if (= 1 (count pairs))
    (first pairs)
    `(do ~@(for [[a b] (->> pairs
                            (remove #{:=>})
                            (partition 2))]
             `(is (= ~a ~b) ~doc)))))

(def interpret-props (partial convert/interpret-props convert/defaults))

#?(:cljs
   (do
     (defn a-fn [] [:div])
     (defn a-constructor []
       (convert/as-element [:div]))))

#?(:cljs
   (do
     (def to-string rdom/renderToStaticMarkup)
     (deftest init-selfhost
       (t/async done
                (selfhost/init!
                 (fn []
                   (selfhost/eval-form
                    '(require '[yawn.selfhost-test-util :as test-util]
                              '[yawn.compiler :as compiler])
                    (fn []
                      (e '(def to-string test-util/to-string))
                      (e '(defn a-fn [] [:div]))
                      (is (fn? (e '(do to-string))))
                      (is (= 2 (e '(+ 1 1))))
                      (done)))))))

     (comment

      (e '(defn macroexpand-seq [form]
            (let [x (macroexpand form)]
              (cons x (lazy-seq
                       (when (not= x form)
                         (recur x)))))))



      ;; expanding elements in selfhost, with type inference
      (e '(defn a-fn [] [:div]))
      (e '(macroexpand '(compiler/as-element [a-fn])))
      (e '(macroexpand '(yawn.infer/maybe-interpret yawn.convert/defaults (a-fn))))

      ;; compile static element to string
      (e '(to-string (compiler/as-element [:div])))

      (e '(require '[yawn.env :as env]
                   'yawn.convert))
      (e '(macroexpand '(env/def-options opts {})))
      (e 'opts)

      (e '(require '[yawn.compiler :as c]))
      (e '(compiler/as-element [:div]))


      (e '(require '[yawn.macros-test-util :as util]))



      (ym/stage :macro :macro :runtime :runtime)

      (ns cljs.user)
      (ym/stage-info)
      (e '(ym/stage-info))
      (e '(ns cljs.user$macros
            (:require [yawn.macros :as ym])))
      (e '(ns cljs.user
            (:require [yawn.macros :as ym])))

      (e '(ns yawn.convert))
      (e 'defaults)
      (e '(ns yawn.convert$macros))
      (e 'defaults)

      (eval-sync '(do to-string))
      (eval-sync 'to-string)
      (eval-sync
       '(require #_'[yawn.compiler :as compiler]
                 '[yawn.convert :as convert]))
      (selfhost/eval-sync
       'convert/defaults)
      (selfhost/eval-sync
               '(macroexpand
                 '(compiler/as-element 1))))))


#?(:cljs
   (deftest macro-tests

     (is
      (thrown? js/Error
               (let [props {:a 1}]
                 (to-string (compiler/as-element [:div props]))))
      "Dynamic props must be annotated for the compiler")

     (is
      (= (let [props {:a 1}]
           (to-string (convert/as-element [:div props])))
         "<div a=\"1\"></div>")
      "Interpreter can handle dynamic props")

     (are [expr html]
       (= (to-string (compiler/as-element expr))
          (to-string (convert/as-element convert/defaults expr))
          (selfhost/eval-sync `(~'to-string (compiler/as-element ~expr)))
          html)

       ;; element
       [:div]
       "<div></div>"

       ;; fn
       (a-fn)
       "<div></div>"

       ;; with classes
       [:div.x.y {:class "z"}]
       "<div class=\"x y z\"></div>"

       ;; dynamic class
       (let [class "c"]
         [:div.a.b {:class class}])
       "<div class=\"a b c\"></div>"

       [:div {:style {:font-size 12}}]
       "<div style=\"font-size:12px\"></div>"

       [:div [a-fn]]
       "<div><div></div></div>"

       [:div (a-fn)]
       "<div><div></div></div>"

       (let [props {:style {:font-size 12}
                    :class "c"}]
         [:div.a.b ^:props props])
       "<div style=\"font-size:12px\" class=\"a b c\"></div>"

       (let [c "c"]
         [:div.a {:class ["b" c]}])
       "<div class=\"a b c\"></div>"

       [:> "div" [:div]]
       "<div><div></div></div>"

       [:<> [:div] [:div]]
       "<div></div><div></div>"

       (do [:div])
       "<div></div>"

       (let [props {:style {:font-size 12}}]
         [:div {:x 1 :& props}])
       "<div x=\"1\" style=\"font-size:12px\"></div>"

       ;; ^:inline
       ;; ^js, inline and as metadata on function
       ;; ^:interpret
       )))

(defn compile [form]
  #?(:clj (compiler/compile form)
     :cljs (e `(compiler/compile ~form))))


(deftest compile-tests

  (-- "DOM tags are compiled to `react/createElement` calls."
      (compile '[:div])
      :=> '(.createElement yawn.react/react "div" nil))

  (-- "Symbol tags are compiled to function calls"
      (compiler/compile '[my-fn 1 2 3])
      :=> '(yawn.infer/maybe-interpret
            yawn.convert/defaults
            (my-fn 1 2 3)))

  (-- "a literal map is compiled to a prop object"
      (compiler/compile '[:span {:class "a"}])
      :=> '(.createElement yawn.react/react "span" (js-obj "className" "a")))

  (-- "a symbol or expression in 1st position is treated as a child element"
      (compiler/compile '[:span a])
      :=> '(.createElement yawn.react/react "span" nil (yawn.infer/maybe-interpret yawn.convert/defaults a))

      (compiler/compile '[:span (a-fn)])
      :=> '(.createElement yawn.react/react "span" nil (yawn.infer/maybe-interpret yawn.convert/defaults (a-fn))))

  (-- "...unless we tag it with :props metadata"
      (compiler/compile-vec '[:span ^:props a])
      :=> '(.createElement yawn.react/react "span" (yawn.convert/interpret-props yawn.convert/defaults a)))

  (-- "keys are camelCase'd"
      (compile-props {:on-click ()})
      :=> {"onClick" ()})

  (-- "class vector is joined at compile time"
      (compile-props {:class ["b" "c"]})
      :=> {"className" "b c"})

  (-- "class vector may include dynamic elements"
      (compile-props '{:class ["b" c]})
      :=> '{"className" (clojure.core/str "b" " " c)})

  (-- "class may be dynamic - with runtime interpretation"
      (compile-props '{:class x})
      :=> '{"className" (yawn.compiler/maybe-interpret-class yawn.convert/defaults x)})

  (-- "classes from tag + props are joined"
      (compiler/compile [:h1.b.c {:class "a"}])
      :=> '(.createElement yawn.react/react "h1" (js-obj "className" "b c a")))

  (-- "joining classes from tag + dynamic class forms"
      (compiler/compile '[:div.c1 {:class x}])
      :=> '(.createElement yawn.react/react
            "div"
            (js-obj "className" (clojure.core/str "c1 " (yawn.compiler/maybe-interpret-class yawn.convert/defaults x))))
      (compiler/compile '[:div.c1 {:class ["y" d]}])
      :=> '(.createElement yawn.react/react
            "div"
            (js-obj "className" (clojure.core/str "c1 " (clojure.core/str "y" " " d)))))
  (-- "style map is also converted to camel-case"
      (compile-props '{:style {:font-weight 600}})
      :=> {"style" {"fontWeight" 600}}
      (compile-props '{:style x})
      :=> '{"style" (yawn.convert/camel-case-keys->obj x)})

  (-- "multiple style maps may be passed (for RN)"
      (compile-props '{:style [{:font-size 10} x]})
      :=> '{"style" [{"fontSize" 10} (yawn.convert/camel-case-keys->obj x)]})

  (-- "special cases of key renaming"
      (keys (compile-props {:for         "htmlFor"          ;; special case
                            :class       "className"        ;; special case
                            :kw-key      "kwKey"            ;; camelCase
                            :aria-key    "aria-key"         ;; not camelCase (aria-*)
                            :data-key    "data-key"         ;; not camelCase (data-*)
                            "string-key" "string-key"       ;; not camelCase (string)
                            }))
      :=> ["htmlFor"
           "className"
           "kwKey"
           "aria-key"
           "data-key"
           "string-key"])

  (-- "a keyword tag is assumed to map to a DOM element and is compiled to createElement"
      (compiler/compile [:div])
      :=> '(.createElement yawn.react/react "div" nil))

  (-- "a symbol tag is assumed to be a regular function that returns a React element.
       this is compiled to a regular function call - with no special handling of \"props\""
      (compiler/compile '[my-fn 1 2 3])
      :=> '(yawn.infer/maybe-interpret yawn.convert/defaults (my-fn 1 2 3)))

  (-- "arguments that look like hiccup forms are compiled unless tagged with ^:inline"
      (compiler/compile '[my-fn [:div]]))
  (compiler/compile '[my-fn [other-fn]])
  (compiler/compile '[my-fn ^:inline [other-fn]])

  ;; to invoke a symbol with createElement (instead of calling it as a function),
  ;; add a ^js hint or use `:>` as the tag
  (-- "invoke a symbol with createElement using ^js metadata"
      (compiler/compile '[^js my-fn])
      :=> '(.createElement yawn.react/react my-fn nil))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure vs React elements


  (-- "invoke a symbol with createElement using :> tag"
      (compiler/compile '[:> my-fn])
      :=> '(.createElement yawn.react/react my-fn nil))

  ;; behind the scenes, `infer/inline?` determines whether a form is already
  ;; a valid React element (in which case, we skip runtime interpretation).
  (def skip? (partial infer/skip? (env/get-opts `convert/defaults)))

  (-- "primitive strings, numbers, and nil are inlined"
      (skip? "a string")
      :=> true
      (skip? 1)
      :=> true
      (skip? nil)
      :=> true
      ;; skip interpretation by adding ^:inline or ^js
      (skip? '^:inline (my-fn))
      :=> true
      (skip? '^js (my-fn))
      :=> true)

  (-- "various ways to control interpretation/inlining of children"

      (compiler/compile '[:div
                          a                                 ;; maybe-interpret (not as props)
                          ^:interpret b                     ;; interpret-form
                          ^:inline c                        ;; inline
                          ^js d])
      '(.createElement yawn.react/react
        "div"
        nil
        (yawn.infer/maybe-interpret yawn.convert/defaults a)
        (yawn.convert/as-element yawn.convert/defaults b)
        c
        d))

  (comment
   ;; using ^js allows us to rely on type propagation, eg. and define functions
   ;; whose return values will be inlined. the following is just an example, and
   ;; will not work here in a Clojure repl.
   (defn ^js my-fn [])
   ;; the ^js type can now be inferred at call sites,
   ;; and will not be wrapped with interpretation functions
   .. (my-fn))

  (-- ":<> compiles to a react Fragment, children are compiled/interpreted"
      (compiler/compile '[:<> [:div]])
      '(.createElement yawn.react/react
        yawn.react/Fragment nil
        (.createElement yawn.react/react "div" nil)))


  (-- "...with a react key"
      (compiler/compile '^{:key "a"} [:<>])
      '(.createElement yawn.react/react yawn.react/Fragment (js-obj "key" "a")))

  (comment
   ;; key as metadata - only on element forms
   (compiler/compile ^{:key 1} [:span "x"])
   (compiler/compile ^{:key 1} [:span {:foo "bar"} "x"])    ;; with props map
   (compiler/compile ^{:key 1} [:span 'd "x"])              ;; with symbol child (unrelated)
   (compiler/compile (quote ^{:key 1} [:span ^:props d "x"])) ;; with dynamic props
   (compiler/compile '(for [x [1 2 3]]
                        ^{:key (:y x)} [:span x]))          ;; dynamic key

   ;; warning - key metadata is ignored because a-symbol is a function
   (compiler/compile '^{:key 1} [a-symbol]))

  (-- "compile inner forms"
      (compiler/compile '(do [:div]))
      '(do (.createElement yawn.react/react "div" nil)))

  (-- "ignore inner forms of unknown operators"
      (compiler/compile '(hello [:div]))
      '(yawn.infer/maybe-interpret yawn.convert/defaults (hello [:div])))

  (comment
   ;; interpret
   (compile-or-interpret-child '(hello [:div]))             ;; maybe-interpret unknown operator

   (compile-or-interpret-child '(let [a 1] [:div a]))       ;; compile inner form, maybe-interpret unknown symbol
   (compile-or-interpret-child '(for [x xs] [:span x]))     ;; compile inner return values
   (compile-or-interpret-child '(let [x (for [x [1 2 3]] x)] ;; only wrap-return at top level
                                  x))



   ;; dynamic props with key and ref added at runtime
   (compiler/compile '^{:key "k" :ref "r"}
                     [:div#id.class ^:props b c])

   (compiler/compile '[:div {:data-style (assoc {} :width 10)}]) ;; random props are not coerced to anything
   ))


(comment
 (e '(require 'yawn.compiler))
 (e '(ns cljs.user (:require [yawn.compiler :as c])))
 (e '(c/compile '[:div]))
 (e 'yawn.react/react)
 (e '(.createElement yawn.react/react "div" nil)))