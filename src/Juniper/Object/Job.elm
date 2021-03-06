-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.Object.Job exposing (..)

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


id : SelectionSet Int Juniper.Object.Job
id =
    Object.selectionForField "Int" "id" [] Decode.int


score : SelectionSet Int Juniper.Object.Job
score =
    Object.selectionForField "Int" "score" [] Decode.int


title : SelectionSet String Juniper.Object.Job
title =
    Object.selectionForField "String" "title" [] Decode.string


url : SelectionSet (Maybe String) Juniper.Object.Job
url =
    Object.selectionForField "(Maybe String)" "url" [] (Decode.string |> Decode.nullable)


text : SelectionSet String Juniper.Object.Job
text =
    Object.selectionForField "String" "text" [] Decode.string


time : SelectionSet Juniper.ScalarCodecs.DateTime Juniper.Object.Job
time =
    Object.selectionForField "ScalarCodecs.DateTime" "time" [] (Juniper.ScalarCodecs.codecs |> Juniper.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


safeText : SelectionSet String Juniper.Object.Job
safeText =
    Object.selectionForField "String" "safeText" [] Decode.string


humanTime : SelectionSet String Juniper.Object.Job
humanTime =
    Object.selectionForField "String" "humanTime" [] Decode.string
