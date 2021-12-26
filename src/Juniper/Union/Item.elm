-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.Union.Item exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode
import Juniper.InputObject
import Juniper.Interface
import Juniper.Object
import Juniper.Scalar
import Juniper.ScalarCodecs
import Juniper.Union


type alias Fragments decodesTo =
    { onStory : SelectionSet decodesTo Juniper.Object.Story
    , onComment : SelectionSet decodesTo Juniper.Object.Comment
    }


{-| Build up a selection for this Union by passing in a Fragments record.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Juniper.Union.Item
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "Story" selections____.onStory
        , Object.buildFragment "Comment" selections____.onComment
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onStory = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onComment = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
