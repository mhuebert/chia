(ns chia.cell-test
  (:require [chia.cell :as cell
             :refer [cell *graph*]
             :refer-macros [defcell cell]]
            [chia.cell.util :as util :refer [id]]
            [cljs.test :refer-macros [deftest
                                      testing
                                      is
                                      are
                                      run-tests
                                      async]]
            [chia.cell.lib]
            [clojure.string :as str]
            [chia.cell.repl :as repl]
            [chia.cell.runtime :as runtime]
            [com.stuartsierra.dependency :as dep]))

(defn dep-set [cells]
  (set (map util/id cells)))

(deftest dependencies-test
          (repl/reset-namespace 'chia.cell-test)

          (let [a (cell 1)
                b (cell @a)
                c (cell @b)
                d (cell c)]

            (are [cell immediate-dependencies]
              (= (dep/immediate-dependencies @*graph* (id cell))
                 (dep-set immediate-dependencies))
              b #{a}
              c #{b}
              d #{})

            (are [cell immediate-dependents]
              (= (dep/immediate-dependents @*graph* (id cell))
                 (dep-set immediate-dependents))
              a #{b}
              b #{c}
              c #{}
              d #{})

            (are [cell transitive-dependents]
              (= (dep/transitive-dependents @*graph* (id cell))
                 (dep-set transitive-dependents))
              a #{b c}
              b #{c}
              c #{}
              d #{})))

    (deftest value-propagation
      (repl/reset-namespace 'chia.cell-test)

      (let [e (cell 1)]

        (is (= 1 @e))

        (reset! e 2)
        (is (= 2 @e))


        (let [f (cell @e)
              g (cell @f)]
          (is (= @e @f @g 2))

          (reset! e 3)
          (is (= @e @f @g 3)))))

    (deftest anonymous-cells
      (repl/reset-namespace 'chia.cell-test)

      (let [h (cell {:key :h} 1)
            i (cell {:key :i} @h)
            j (cell (str @h @i))]

        (= (dep/immediate-dependents @*graph* (id h)) (dep-set #{i}))
        (is (= 1 @i))

        (is (= "11" @j))

        (reset! h 2)
        (is (= "22" @j))


        (doseq [key ["a" :b 1 {} []]]
          (let [c (cell {:key key} nil)]
            (is (str/includes? (name (util/id c))
                               (str (hash key)))
                "cell contains hash of name argument")))

        (testing "Anonymous cells in a loop"
          (let [multi (for [n [1 2 3 4 5]]
                        (cell {:key n} n))]
            (is (= (list 1 2 3 4 5)
                   (map deref multi)))))))

    (deftest cell-function
      (repl/reset-namespace 'chia.cell-test)

      (let [o (cell (mapv cell (map (partial hash-map :key) (range)) [:a :b]))]
        (is (= (mapv deref @o) [:a :b])
            "When given unique keys, cell function returns unique cells"))

      (let [p (cell (mapv cell (map (partial hash-map :key) (repeat 1)) [:c :d]))]
        (is (= (mapv deref @p) [:c :c])
            "When given same key, cell function returns existing cell")))

(deftest contexts
  ;; We keep track of the "evaluation context" of a cell, such as the
  ;; code editor window where the cell is defined, to facilitate
  ;; interactive development.

  ;; Then we can dispose of all the cells 'owned' by that context when
  ;; desired, eg. immediately before the user has made a change to the
  ;; source code in a particular block of code, and wants to re-evaluate it.

  (repl/reset-namespace 'chia.cell-test)


  (let [ctx-1 (runtime/default-runtime)
        ctx-2 (runtime/default-runtime)]

    (binding [runtime/*runtime* ctx-1]
      (defcell r 1)
      (defcell r- @r))

    (binding [runtime/*runtime* ctx-2]
      (defcell s @r)
      (defcell s- @s))

    (is (= (dep/transitive-dependents @*graph* (id r))
           (dep-set [r- s s-])))

    (runtime/dispose! ctx-1)

    (is (= nil @r @r-))
    (is (= 1 @s @s-))

    (is (= (dep/transitive-dependents @*graph* (id r))
           (dep-set [r- s s-]))
        "Dependencies to named cells persist"))



  (comment (binding [runtime/*runtime* ctx-1
                     cell/DEBUG true]
             (cell/eval-cell! r)
             (cell/eval-cell! r-))

           (binding [runtime/*runtime* ctx-2
                     cell/DEBUG true]
             (cell/eval-cell! s)
             (cell/eval-cell! s-)))



  (def ctx-3 (runtime/default-runtime))
  (def ctx-4 (runtime/default-runtime))

  (binding [runtime/*runtime* ctx-3]
    (defn f1 [x]
      (cell @(cell {:key x} (str x 1)))))

  (defn f2 []
    (def f3 (cell @(f1 "X"))))
  (binding [runtime/*runtime* ctx-4]
    (f2))

  (is (= "X1" @f3))
  (runtime/dispose! ctx-4)
  (is (= nil @f3))
  (f2)
  (is (= "X1" @f3))

  )

#_(comment
   (deftest timers
     ;; need a better approach for automated testing of interval
     (repl/reset-namespace 'chia.cell-test)

     (let [n (cell (interval 1000 inc))
           o (cell (interval 1500 inc))
           p (cell (str @n ", " @o))]

       (cell (prn @p))))

   (defcell birds
            (lib/fetch "https://ebird.org/ws1.1/data/obs/geo/recent"
                       {:query {:lat "52.4821146"
                                :lng "13.4121388"
                                :maxResults 10
                                :fmt "json"}}))

   (defcell first-bird
            (first @birds)))
