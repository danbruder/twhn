module Selections exposing (..)

import Api
import Domain.Comment exposing (Comment)
import Domain.Item exposing (Item(..))
import Domain.Story exposing (Story)
import Effect exposing (Effect)
import Gen.Params.Comments.Id_ exposing (Params)
import Gen.Params.Items.Id_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Juniper.Object.Comment as Comment
import Juniper.Object.Story as Story
import Juniper.Query as Query
import Juniper.Union
import Juniper.Union.Item as Item
import Page
import Request
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Ui
import Url exposing (Url)
import Util
import View exposing (View)


item : SelectionSet Item Juniper.Union.Item
item =
    Item.fragments
        { onComment =
            SelectionSet.map Item__Comment
                (SelectionSet.succeed Comment
                    |> with Comment.id
                    |> with Comment.safeText
                    |> with Comment.by
                    |> with Comment.humanTime
                    |> with Comment.parent
                    |> with (SelectionSet.withDefault [] Comment.kids)
                )
        , onStory =
            SelectionSet.map Item__Story
                (SelectionSet.succeed Story
                    |> with Story.id
                    |> with Story.title
                    |> with (SelectionSet.map (Maybe.withDefault "" >> Url.fromString) Story.url)
                    |> with Story.by
                    |> with Story.score
                    |> with Story.humanTime
                    |> with (SelectionSet.withDefault [] Story.kids)
                )
        }
