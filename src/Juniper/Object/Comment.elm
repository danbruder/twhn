-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.Object.Comment exposing (..)

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


id : SelectionSet Int Juniper.Object.Comment
id =
    Object.selectionForField "Int" "id" [] Decode.int


by : SelectionSet String Juniper.Object.Comment
by =
    Object.selectionForField "String" "by" [] Decode.string


kids : SelectionSet (Maybe (List Int)) Juniper.Object.Comment
kids =
    Object.selectionForField "(Maybe (List Int))" "kids" [] (Decode.int |> Decode.list |> Decode.nullable)


parent : SelectionSet Int Juniper.Object.Comment
parent =
    Object.selectionForField "Int" "parent" [] Decode.int


text : SelectionSet String Juniper.Object.Comment
text =
    Object.selectionForField "String" "text" [] Decode.string


time : SelectionSet Juniper.ScalarCodecs.DateTime Juniper.Object.Comment
time =
    Object.selectionForField "ScalarCodecs.DateTime" "time" [] (Juniper.ScalarCodecs.codecs |> Juniper.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


children :
    SelectionSet decodesTo Juniper.Union.Item
    -> SelectionSet (List decodesTo) Juniper.Object.Comment
children object____ =
    Object.selectionForCompositeField "children" [] object____ (Basics.identity >> Decode.list)


ancestors :
    SelectionSet decodesTo Juniper.Union.Item
    -> SelectionSet (List decodesTo) Juniper.Object.Comment
ancestors object____ =
    Object.selectionForCompositeField "ancestors" [] object____ (Basics.identity >> Decode.list)


descendants :
    SelectionSet decodesTo Juniper.Union.Item
    -> SelectionSet (List decodesTo) Juniper.Object.Comment
descendants object____ =
    Object.selectionForCompositeField "descendants" [] object____ (Basics.identity >> Decode.list)


safeText : SelectionSet String Juniper.Object.Comment
safeText =
    Object.selectionForField "String" "safeText" [] Decode.string


humanTime : SelectionSet String Juniper.Object.Comment
humanTime =
    Object.selectionForField "String" "humanTime" [] Decode.string


isBookmarked : SelectionSet Bool Juniper.Object.Comment
isBookmarked =
    Object.selectionForField "Bool" "isBookmarked" [] Decode.bool
