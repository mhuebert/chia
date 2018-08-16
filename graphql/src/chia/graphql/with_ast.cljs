(ns chia.graphql.with-ast
  (:require ["graphql/language/parser" :as parser]
            ["graphql/utilities/concatAST" :refer [concatAST]]
            [chia.graphql :as g]
            [chia.util.js-interop :as j]
            [chia.util :as u]))

(defmethod g/graphql-lookup :ast
  [GQL _ _]
  (u/memoized-on GQL :ast
    (delay
     (-> @(:string GQL)
         (parser/parse #js {:noLocation true})))))

(defmethod g/graphql-lookup :ast+
  [GQL _ _]
  (u/memoized-on GQL :ast+
    (delay
     (->> @(:fragments+ GQL)
          (reduce (fn [out {fragment-ast :ast}]
                    (j/push! out @fragment-ast))
                  #js [@(:ast GQL)])
          (concatAST)))))

(extend-type g/GraphQL
  IDeref
  (-deref [this] @(:ast+ this)))