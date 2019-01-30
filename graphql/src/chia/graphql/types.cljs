(ns chia.graphql.types
  (:refer-clojure :exclude [List Object type])
  (:require ["graphql/type" :as graphql]
            [chia.util.js-interop :as j]
            [chia.util :as u]
            [cljs.core :as core]))

(def String graphql/GraphQLString)
(def Int graphql/GraphQLInt)
(def Float graphql/GraphQLFloat)
(def Boolean graphql/GraphQLBoolean)
(def ID graphql/GraphQLID)
(def NonNull graphql/GraphQLNonNull)
(def Object graphql/GraphQLObjectType)
(def InputObject graphql/GraphQLInputObjectType)
(def Interface graphql/GraphQLInterfaceType)
(def Union graphql/GraphQLUnionType)
(def Enum graphql/GraphQLEnumType)
(def List graphql/GraphQLList)
(def Schema graphql/GraphQLSchema)

(defn scalar [k]
  (case k
    :String String
    :Int Int
    :Float Float
    :Boolean Boolean
    :ID ID
    nil))

(defn composite [k]
  (case k
    :Object Object
    :InputObject InputObject
    :Interface Interface
    :Union Union
    :Enum Enum
    nil))

(defn builtin [k]
  (or (scalar k)
      (composite k)))

(def type? graphql/isType)
