-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.Object.Stats exposing (..)

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


itemCount : SelectionSet Int Juniper.Object.Stats
itemCount =
    Object.selectionForField "Int" "itemCount" [] Decode.int


minItemId : SelectionSet Int Juniper.Object.Stats
minItemId =
    Object.selectionForField "Int" "minItemId" [] Decode.int


maxItemId : SelectionSet Int Juniper.Object.Stats
maxItemId =
    Object.selectionForField "Int" "maxItemId" [] Decode.int
