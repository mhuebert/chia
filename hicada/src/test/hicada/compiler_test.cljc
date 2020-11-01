(ns hicada.compiler-test
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :refer [deftest is are]]
            [hicada.compiler :as c :refer [compile
                                           compile-props
                                           compile-vec]]))

(defmacro -- [doc & pairs]
  (if (= 1 (count pairs))
    (first pairs)
    `(do ~@(for [[a b] (->> pairs
                            (remove #{:=>})
                            (partition 2))]
             `(is (= ~a ~b) ~doc)))))

(deftest compile-tests

  (-- "DOM tags are compiled to `react/createElement` calls."
      (compile '[:div])
      :=> '(hicada.runtime/createElement "div" nil))

  (-- "Symbol tags are compiled to function calls"
      (compile '[my-fn 1 2 3])
      :=> '(hicada.infer/maybe-interpret
             (my-fn 1 2 3)))

  (-- "a literal map is compiled to a prop object"
      (compile '[:span {:class "a"}])
      :=> '(hicada.runtime/createElement
             "span"
             (js* "{'className':~{}}" "a")))

  (-- "a symbol or expression in 1st position is treated as a child element"
      (compile '[:span a])
      :=> '(hicada.runtime/createElement "span" nil (hicada.infer/maybe-interpret a))

      (compile '[:span (a-fn)])
      :=> '(hicada.runtime/createElement "span" nil (hicada.infer/maybe-interpret (a-fn))))

  (-- "...unless we tag it with :props metadata"
      (compile-vec '[:span ^:props a])
      :=> '(hicada.runtime/createElement "span" (hicada.runtime/props a)))

  (-- "keys are camelCase'd"
      (compile-props {:on-click ()})
      :=> {"onClick" ()})

  (-- "class vector is joined at compile time"
      (compile-props {:class ["b" "c"]})
      :=> {"className" "b c"})

  (-- "class vector may include dynamic elements"
      (compile-props '{:class ["b" c]})
      :=> '{"className" (clojure.string/join " " [["b" c]])})

  (-- "class may be dynamic - with runtime interpretation"
      (compile-props '{:class x})
      :=> '{"className" (hicada.compiler/ensure-class-string x)})

  (-- "classes from tag + props are joined"
      (compile [:h1.b.c {:class "a"}])
      :=> '(hicada.runtime/createElement "h1" (js* "{'className':~{}}" "a b c")))

  (-- "warning - :class-name is overwritten by static class"
      (compile [:h1.a {:class-name "b"}])
      :=> '(hicada.runtime/createElement "h1" (js* "{'className':~{}}" "a")))

  (-- "joining classes from tag + dynamic class forms"
      (compile '[:div.c1 {:class x}])
      :=> '(hicada.runtime/createElement
            "div"
            (js*
              "{'className':~{}}"
              (hicada.compiler/ensure-class-string
                (clojure.core/str (hicada.compiler/ensure-class-string x) " " "c1")))))

  (comment



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Props


    (compile '[:div.c1 {:class ["y" d]}])

    ;; style map is also converted to camel-case
    (compile-props '{:style {:font-weight 600}})
    ;; style map may be dynamic - with runtime interpretation
    (compile-props '{:style x})
    (compile '[:div {:style (assoc {} :width 10)}])
    ;; multiple style maps may be passed (for RN)
    (compile-props '{:style [{:font-size 10} x]})

    ;; some keys are handled as special cases when renamed
    (compile-props {:for "htmlFor"                          ;; special case
                    :class "className"                      ;; special case
                    :kw-key "kwKey"                         ;; camelCase
                    :aria-key "aria-key"                    ;; not camelCase (aria-*)
                    :data-key "data-key"                    ;; not camelCase (data-*)
                    "string-key" "string-key"               ;; not camelCase (string)
                    })

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Clojure vs React elements

    ;; a keyword tag is assumed to map to a DOM element and is compiled to createElement
    (compile-vec [:div])
    => (hicada.runtime/createElement "div" nil)

    ;; a symbol tag is assumed to be a regular function that returns a React element.
    ;; this is compiled to a regular function call - with no special handling of "props"
    (compile-vec '[my-fn 1 2 3])
    => (my-fn 1 2 3)

    ;; actually, the result is wrapped in `hicada.infer/maybe-interpret`.
    => (hicada.infer/maybe-interpret (my-fn 1 2 3))
    ;; more on that later.

    ;; arguments that look like hiccup forms are compiled unless tagged with ^:inline
    (compile-vec '[my-fn [:div]])
    (compile-vec '[my-fn [other-fn]])
    (compile-vec '[my-fn ^:inline [other-fn]])

    ;; to invoke a symbol with createElement (instead of calling it as a function),
    ;; add a ^js hint or use `:>` as the tag
    (compile-vec '[^js my-fn])
    (compile-vec '[:> my-fn])
    => (hicada.runtime/createElement my-fn nil)

    ;; behind the scenes, `infer/inline?` determines whether a form is already
    ;; a valid React element (in which case, we skip runtime interpretation).

    ;; primitive strings, numbers, and nil are inlined:
    (infer/inline? "a string")                              ;; true
    (infer/inline? 1)                                       ;; true
    (infer/inline? nil)                                     ;; true
    ;; skip interpretation by adding ^:inline or ^js
    (infer/inline? '^:inline (my-fn))                       ;; true
    (infer/inline? '^js (my-fn))                            ;; true

    ;; using ^js allows us to rely on type propagation, eg. and define functions
    ;; whose return values will be inlined. the following is just an example, and
    ;; will not work here in a Clojure repl.
    (defn ^js my-fn [])
    ;; the ^js type can now be inferred at call sites,
    ;; and will not be wrapped with interpretation functions
    .. (my-fn)


    ;; Here are some more complete examples that include props and children.
    (compile-vec '[my-fn {:foo "bar"} a [:div]])
    (compile-vec '[:> my-fn {:foo "bar"} a [:div]])
    (compile-vec '[^js my-fn {:foo "bar"} a [:div]])

    ;; various ways to control interpretation/inlining of children
    (compile-vec '[:div
                   a                                        ;; maybe-interpret (not as props)
                   ^:interpret b                            ;; interpret-form
                   ^:inline c                               ;; inline
                   ^js d])                                  ;; inline

    ;; :<> compiles to a react Fragment, children are compiled/interpreted
    (compile-vec '[:<> a b [:div] [x]])
    ;; ...with a react key
    (compile '^{:key "a"} [:<> a b])

    ;; interpret
    (compile-or-interpret-child 'b)                         ;; maybe-interpret symbol
    (compile-or-interpret-child '(hello [:div]))            ;; maybe-interpret unknown operator
    (compile-or-interpret-child '(do [:div]))               ;; compile inner form
    (compile-or-interpret-child '(let [a 1] [:div a]))      ;; compile inner form, maybe-interpret unknown symbol
    (compile-or-interpret-child '(for [x xs] [:span x]))    ;; compile inner return values
    (compile-or-interpret-child '(let [x (for [x [1 2 3]] x)] ;; only wrap-return at top level
                                   x))

    ;; key as metadata - only on element forms
    (compile-vec ^{:key 1} [:span "x"])
    (compile-vec ^{:key 1} [:span {:foo "bar"} "x"])        ;; with props map
    (compile-vec ^{:key 1} [:span 'd "x"])                  ;; with symbol child (unrelated)
    (compile-vec (quote ^{:key 1} [:span ^:props d "x"]))   ;; with dynamic props
    (compile '(for [x [1 2 3]]
                ^{:key (:y x)} [:span x]))                  ;; dynamic key

    ;; warning - key metadata is ignored because a-symbol is a function
    (compile-vec '^{:key 1} [a-symbol])

    ;; dynamic props with key and ref added at runtime
    (compile '^{:key "k" :ref "r"}
             [:div#id.class ^:props b c])

    (compile '[:div {:data-style (assoc {} :width 10)}])    ;; random props are not coerced to anything

    )

  )

(comment

  ;; High-Level Overview
  ;;
  ;; DOM tags are compiled to `react/createElement` calls.
  (compile '[:div])
  => (hicada.runtime/createElement "div" nil)

  ;; Symbol tags are compiled to function calls
  (compile '[my-fn 1 2 3])
  => (my-fn 1 2 3)

  ;; a literal map is compiled as props
  (compile '[:span {:on-click #()}])

  ;; a symbol or expression in 1st position is treated as a child element
  (compile '[:span a])
  (compile '[:span (a-fn)])

  ;; ...unless we tag it with :props metadata
  (compile-vec '[:span ^:props a])
  (compile-vec '[:span ^:props (a-fn)])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Props

  ;; keys are camelCase'd
  (compile-props {:on-click ()})

  ;; class vector is joined at compile time
  (compile-props {:class ["b" "c"]})
  ;; class vector may include dynamic elements
  (compile-props '{:class ["b" c]})
  ;; class may be dynamic - with runtime interpretation
  (compile-props '{:class x})
  ;; classes from tag + props are joined
  (compile [:h1.b.c {:class "a"}])
  ;; warning - :class-name is ignored
  (compile [:h1.b.c {:class-name "a"}])
  ;; joining classes from tag + dynamic class forms
  (compile '[:div.c1 {:class x} "D"])
  (compile '[:div.c1 {:class ["y" d]}])

  ;; style map is also converted to camel-case
  (compile-props '{:style {:font-weight 600}})
  ;; style map may be dynamic - with runtime interpretation
  (compile-props '{:style x})
  (compile '[:div {:style (assoc {} :width 10)}])
  ;; multiple style maps may be passed (for RN)
  (compile-props '{:style [{:font-size 10} x]})

  ;; some keys are handled as special cases when renamed
  (compile-props {:for "htmlFor"                            ;; special case
                  :class "className"                        ;; special case
                  :kw-key "kwKey"                           ;; camelCase
                  :aria-key "aria-key"                      ;; not camelCase (aria-*)
                  :data-key "data-key"                      ;; not camelCase (data-*)
                  "string-key" "string-key"                 ;; not camelCase (string)
                  })

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure vs React elements

  ;; a keyword tag is assumed to map to a DOM element and is compiled to createElement
  (compile-vec [:div])
  => (hicada.runtime/createElement "div" nil)

  ;; a symbol tag is assumed to be a regular function that returns a React element.
  ;; this is compiled to a regular function call - with no special handling of "props"
  (compile-vec '[my-fn 1 2 3])
  => (my-fn 1 2 3)

  ;; actually, the result is wrapped in `hicada.infer/maybe-interpret`.
  => (hicada.infer/maybe-interpret (my-fn 1 2 3))
  ;; more on that later.

  ;; arguments that look like hiccup forms are compiled unless tagged with ^:inline
  (compile-vec '[my-fn [:div]])
  (compile-vec '[my-fn [other-fn]])
  (compile-vec '[my-fn ^:inline [other-fn]])

  ;; to invoke a symbol with createElement (instead of calling it as a function),
  ;; add a ^js hint or use `:>` as the tag
  (compile-vec '[^js my-fn])
  (compile-vec '[:> my-fn])
  => (hicada.runtime/createElement my-fn nil)

  ;; behind the scenes, `infer/inline?` determines whether a form is already
  ;; a valid React element (in which case, we skip runtime interpretation).

  ;; primitive strings, numbers, and nil are inlined:
  (infer/inline? "a string")                                ;; true
  (infer/inline? 1)                                         ;; true
  (infer/inline? nil)                                       ;; true
  ;; skip interpretation by adding ^:inline or ^js
  (infer/inline? '^:inline (my-fn))                         ;; true
  (infer/inline? '^js (my-fn))                              ;; true

  ;; using ^js allows us to rely on type propagation, eg. and define functions
  ;; whose return values will be inlined. the following is just an example, and
  ;; will not work here in a Clojure repl.
  (defn ^js my-fn [])
  ;; the ^js type can now be inferred at call sites,
  ;; and will not be wrapped with interpretation functions
  .. (my-fn)


  ;; Here are some more complete examples that include props and children.
  (compile-vec '[my-fn {:foo "bar"} a [:div]])
  (compile-vec '[:> my-fn {:foo "bar"} a [:div]])
  (compile-vec '[^js my-fn {:foo "bar"} a [:div]])

  ;; various ways to control interpretation/inlining of children
  (compile-vec '[:div
                 a                                          ;; maybe-interpret (not as props)
                 ^:interpret b                              ;; interpret-form
                 ^:inline c                                 ;; inline
                 ^js d])                                    ;; inline

  ;; :<> compiles to a react Fragment, children are compiled/interpreted
  (compile-vec '[:<> a b [:div] [x]])
  ;; ...with a react key
  (compile '^{:key "a"} [:<> a b])

  ;; interpret
  (compile-or-interpret-child 'b)                           ;; maybe-interpret symbol
  (compile-or-interpret-child '(hello [:div]))              ;; maybe-interpret unknown operator
  (compile-or-interpret-child '(do [:div]))                 ;; compile inner form
  (compile-or-interpret-child '(let [a 1] [:div a]))        ;; compile inner form, maybe-interpret unknown symbol
  (compile-or-interpret-child '(for [x xs] [:span x]))      ;; compile inner return values
  (compile-or-interpret-child '(let [x (for [x [1 2 3]] x)] ;; only wrap-return at top level
                                 x))

  ;; key as metadata - only on element forms
  (compile-vec ^{:key 1} [:span "x"])
  (compile-vec ^{:key 1} [:span {:foo "bar"} "x"])          ;; with props map
  (compile-vec ^{:key 1} [:span 'd "x"])                    ;; with symbol child (unrelated)
  (compile-vec (quote ^{:key 1} [:span ^:props d "x"]))     ;; with dynamic props
  (compile '(for [x [1 2 3]]
              ^{:key (:y x)} [:span x]))                    ;; dynamic key

  ;; warning - key metadata is ignored because a-symbol is a function
  (compile-vec '^{:key 1} [a-symbol])

  ;; dynamic props with key and ref added at runtime
  (compile '^{:key "k" :ref "r"}
           [:div#id.class ^:props b c])

  (compile '[:div {:data-style (assoc {} :width 10)}])      ;; random props are not coerced to anything

  )


