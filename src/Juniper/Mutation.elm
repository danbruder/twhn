-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.Mutation exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Juniper.InputObject
import Juniper.Interface
import Juniper.Object
import Juniper.Scalar
import Juniper.ScalarCodecs
import Juniper.Union


type alias BookmarkItemRequiredArguments =
    { itemId : Int }


bookmarkItem :
    BookmarkItemRequiredArguments
    -> SelectionSet decodesTo Juniper.Union.Item
    -> SelectionSet (Maybe decodesTo) RootMutation
bookmarkItem requiredArgs____ object____ =
    Object.selectionForCompositeField "bookmarkItem" [ Argument.required "itemId" requiredArgs____.itemId Encode.int ] object____ (Basics.identity >> Decode.nullable)


type alias UnbookmarkItemRequiredArguments =
    { itemId : Int }


unbookmarkItem :
    UnbookmarkItemRequiredArguments
    -> SelectionSet decodesTo Juniper.Union.Item
    -> SelectionSet (Maybe decodesTo) RootMutation
unbookmarkItem requiredArgs____ object____ =
    Object.selectionForCompositeField "unbookmarkItem" [ Argument.required "itemId" requiredArgs____.itemId Encode.int ] object____ (Basics.identity >> Decode.nullable)
