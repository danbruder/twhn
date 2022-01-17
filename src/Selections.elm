module Selections exposing (..)

import Api
import Domain.Comment exposing (Comment)
import Domain.Item as Item exposing (Item(..))
import Domain.Job exposing (Job)
import Domain.Story exposing (Story)
import Effect exposing (Effect)
import Gen.Params.Items.Id_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Iso8601
import Juniper.Object
import Juniper.Object.Comment as Comment
import Juniper.Object.ItemMetric as ItemMetric
import Juniper.Object.Job as Job
import Juniper.Object.Story as Story
import Juniper.Query as Query
import Juniper.Scalar exposing (..)
import Juniper.Union
import Juniper.Union.Item as ItemUnion
import Page
import Request
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Time exposing (Posix)
import Ui
import Url exposing (Url)
import Util
import View exposing (View)


item : SelectionSet Item Juniper.Union.Item
item =
    ItemUnion.fragments
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
                    |> with Story.safeText
                    |> with (SelectionSet.map (Maybe.withDefault "" >> Url.fromString) Story.url)
                    |> with Story.by
                    |> with Story.score
                    |> with Story.humanTime
                    |> with (SelectionSet.withDefault [] Story.kids)
                    |> with
                        (SelectionSet.map
                            (\ranks ->
                                ranks
                                    |> List.filter (\r -> r.value <= 30)
                            )
                            (Story.rank rankSelection)
                        )
                )
        , onJob =
            SelectionSet.map Item__Job
                (SelectionSet.succeed Job
                    |> with Job.id
                    |> with Job.score
                    |> with Job.safeText
                    |> with (SelectionSet.map (Maybe.withDefault "" >> Url.fromString) Job.url)
                    |> with Job.humanTime
                    |> with Job.title
                )
        }


children : SelectionSet (List Item) Juniper.Union.Item
children =
    ItemUnion.fragments
        { onComment =
            SelectionSet.succeed identity
                |> with (Comment.children item)
        , onStory =
            SelectionSet.succeed identity
                |> with (Story.children item)
        , onJob = SelectionSet.succeed identity |> hardcoded []
        }


descendants : SelectionSet (List Item) Juniper.Union.Item
descendants =
    ItemUnion.fragments
        { onComment = Comment.descendants item
        , onStory = Story.descendants item
        , onJob = SelectionSet.succeed identity |> hardcoded []
        }


ancestors : SelectionSet (List Item) Juniper.Union.Item
ancestors =
    ItemUnion.fragments
        { onComment =
            SelectionSet.succeed identity
                |> with (Comment.ancestors item)
        , onStory = SelectionSet.succeed identity |> hardcoded []
        , onJob = SelectionSet.succeed identity |> hardcoded []
        }


rankSelection : SelectionSet Domain.Story.Rank Juniper.Object.ItemMetric
rankSelection =
    SelectionSet.succeed Domain.Story.Rank
        |> with ItemMetric.value
        |> with (SelectionSet.mapOrFail parseTheDate ItemMetric.createdAt)


parseTheDate : Juniper.Scalar.NaiveDateTime -> Result String Posix
parseTheDate (NaiveDateTime dateStr) =
    dateStr
        |> Iso8601.toTime
        |> Result.mapError (always "")
