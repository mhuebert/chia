(ns yawn.selfhost-test
  (:require [cljs.js :as cljs]
            [shadow.cljs.bootstrap.browser :as shadow.bootstrap]
            [cljs.test :as test :refer [deftest is async]]
            [yawn.compiler :as compiler]
            #?(:cljs ["react-dom/server" :as rdom])))

#?(:cljs
   (def to-string rdom/to))

(def c-state (cljs/empty-state))

(defn init! [cb]
  (shadow.bootstrap/init c-state
                         {:path         "/selfhost"
                          :load-on-init '#{yawn.compiler}}
                         #(cb)))


(defn eval-form [form cb]
  (let [options {:eval    cljs/js-eval
                 :ns (::ns @c-state (symbol "cljs.user"))
                 ;; use the :load function provided by shadow-cljs, which uses the bootstrap build's
                 ;; index.transit.json file to map namespaces to files.
                 :load    (partial shadow.bootstrap/load c-state)
                 :context :expr}
        f (fn [x] (when (:error x)
                    (prn :error form)
                    (js/console.error (ex-cause (:error x))))
            (cb x))]
    (cljs/eval c-state form options f)))

(defn compile-str [source cb]
  (let [options {:eval    cljs/js-eval
                 :ns (::ns @c-state (symbol "cljs.user"))
                 ;; use the :load function provided by shadow-cljs, which uses the bootstrap build's
                 ;; index.transit.json file to map namespaces to files.
                 :load    (partial shadow.bootstrap/load c-state)
                 :context :expr}
        f (fn [x] (when (:error x)
                    (js/console.error (ex-cause (:error x))))
            (cb x))]
    (cljs/compile-str c-state (str source) "[test]" options f)))

(defn eval-sync [source]
  (let [a (atom nil)]
    (eval-form source #(reset! a %))
    (when (:ns @a)
      (swap! c-state assoc ::ns (:ns @a)))
    (:value @a)))

(defn compile-sync [source]
  (let [a (atom nil)]
    (compile-str source #(reset! a %))
    (when (:ns @a)
      (swap! c-state assoc ::ns (:ns @a)))
    (:value @a)))

#_(deftest init
  (async done
    (init! (fn [& args]
             (is (= 2 (:value (eval-sync '(+ 1 1)))))
             (done)))))




