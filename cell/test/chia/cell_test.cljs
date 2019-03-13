(ns chia.cell-test
  (:require [chia.cell :as cell :refer [cell] :refer-macros [defcell cell]]
            [chia.cell.util :as util]
            [cljs.test :refer-macros [deftest
                                      testing
                                      is
                                      are
                                      run-tests
                                      async]]
            [chia.cell.lib]
            [clojure.string :as str]))

(defn dep-set [cells]
  (set cells))

(deftest dependencies-test
  (let [a (cell 1)
        b (cell @a)
        c (cell @b)
        d (cell c)]

    (are [cell immediate-dependencies]
      (= (set (cell/immediate-dependencies cell))
         (dep-set immediate-dependencies))
      b #{a}
      c #{b}
      d #{})

    (are [cell immediate-dependents]
      (= (set (cell/immediate-dependents cell))
         (dep-set immediate-dependents))
      a #{b}
      b #{c}
      c #{}
      d #{})

    (are [cell transitive-dependents]
      (= (set (cell/transitive-dependents cell))
         (dep-set transitive-dependents))
      a #{b c}
      b #{c}
      c #{}
      d #{})))

(deftest value-propagation

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

  (let [h (cell {:key :h} 1)
        i (cell {:key :i} @h)
        j (cell (str @h @i))]

    (= (set (cell/immediate-dependents h)) (dep-set #{i}))
    (is (= 1 @i))

    (is (= "11" @j))

    (reset! h 2)
    (is (= "22" @j))


    (doseq [key ["a" :b 1 {} []]]
      (let [c (cell {:key key} nil)]
        (is (str/includes? (name (.-id c))
                           (str (hash key)))
            "cell contains hash of name argument")))

    (testing "Anonymous cells in a loop"
      (let [multi (for [n [1 2 3 4 5]]
                    (cell {:key n} n))]
        (is (= (list 1 2 3 4 5)
               (map deref multi)))))))

(deftest cell-function

  (let [o (cell (mapv cell (map (partial hash-map :key) (range)) [:a :b]))]
    (is (= (mapv deref @o) [:a :b])
        "When given unique keys, cell function returns unique cells"))

  (let [p (cell (mapv cell (map (partial hash-map :key) (repeat 1)) [:c :d]))]
    (is (= (mapv deref @p) [:c :c])
        "When given same key, cell function returns existing cell")))

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
