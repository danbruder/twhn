-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.Object.ItemMetric exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Juniper.InputObject
import Juniper.Interface
import Juniper.Object
import Juniper.Scalar
import Juniper.ScalarCodecs
import Juniper.Union


itemId : SelectionSet Int Juniper.Object.ItemMetric
itemId =
    Object.selectionForField "Int" "itemId" [] Decode.int


metric : SelectionSet String Juniper.Object.ItemMetric
metric =
    Object.selectionForField "String" "metric" [] Decode.string


value : SelectionSet Int Juniper.Object.ItemMetric
value =
    Object.selectionForField "Int" "value" [] Decode.int


createdAt : SelectionSet Juniper.ScalarCodecs.NaiveDateTime Juniper.Object.ItemMetric
createdAt =
    Object.selectionForField "ScalarCodecs.NaiveDateTime" "createdAt" [] (Juniper.ScalarCodecs.codecs |> Juniper.Scalar.unwrapCodecs |> .codecNaiveDateTime |> .decoder)
