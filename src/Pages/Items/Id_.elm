module Pages.Items.Id_ exposing (Model, Msg, page)

import Api
import Browser.Dom as Dom
import Css
import Dict exposing (Dict)
import Domain.Comment exposing (Comment)
import Domain.Item as Item exposing (Item(..))
import Domain.Story exposing (Story)
import Effect exposing (Effect)
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
import Selections
import Set exposing (Set)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Task
import Tree exposing (Tree)
import Tree.Zipper
import Ui
import Url exposing (Url)
import Util
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req.params.id
        , update = update
        , view = view req.route
        , subscriptions = always Sub.none
        }


type alias Thread =
    Tree Item


buildThread : Item -> Dict Int Item -> Thread
buildThread root items =
    let
        handleKids item =
            Item.kids item
                |> List.filterMap (\id -> Dict.get id items)
                |> List.map (\i -> Tree.tree i (handleKids i))
    in
    Tree.tree root (handleKids root)



-- INIT


type alias Model =
    { status : Status
    }


type Status
    = Loading
    | NotFound
    | Loaded Thread


init : Shared.Model -> String -> ( Model, Effect Msg )
init shared idStr =
    case String.toInt idStr of
        Just id ->
            case Dict.get id shared.items of
                Just item ->
                    ( { status = Loaded (buildThread item shared.items)
                      }
                    , [ boot id, resetViewport ]
                        |> Effect.batch
                    )

                Nothing ->
                    ( { status = Loading
                      }
                    , boot id
                    )

        _ ->
            ( { status = NotFound
              }
            , Effect.none
            )


resetViewport : Effect Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)
        |> Effect.fromCmd


type alias Response =
    { item : Maybe Item
    , children : Maybe (List Item)
    , descendants : Maybe (List Item)
    , ancestors : Maybe (List Item)
    }


boot : Int -> Effect Msg
boot id =
    SelectionSet.map4
        (\maybeItem maybeChildren maybeDescendants maybeAncestors ->
            { item = maybeItem
            , children = maybeChildren
            , descendants = maybeDescendants
            , ancestors = maybeAncestors
            }
        )
        (Query.itemById { id = id } Selections.item)
        (Query.itemById { id = id } Selections.children)
        (Query.itemById { id = id } Selections.descendants)
        (Query.itemById { id = id } Selections.ancestors)
        |> Api.makeRequest GotItem
        |> Effect.fromCmd



-- UPDATE


type Msg
    = GotItem (Result (Graphql.Http.Error Response) Response)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        GotItem response ->
            case response of
                Ok payload ->
                    case payload.item of
                        Just item ->
                            let
                                items =
                                    [ payload.children
                                    , payload.descendants
                                    , payload.ancestors
                                    ]
                                        |> List.filterMap identity
                                        |> List.concat
                                        |> List.map (\item_ -> ( Item.id item_, item_ ))
                                        |> Dict.fromList
                            in
                            ( { model | status = Loaded (buildThread item items) }
                            , Shared.gotItems (item :: Dict.values items) |> Effect.fromShared
                            )

                        _ ->
                            ( { model | status = NotFound }, Effect.none )

                Err _ ->
                    ( model, Effect.none )



-- VIEW


view : Route -> Model -> View msg
view route model =
    let
        title =
            case model.status of
                Loaded thread ->
                    if Item.isComment (Tree.label thread) then
                        "Comment"

                    else if Item.isStory (Tree.label thread) then
                        "Story"

                    else
                        ""

                _ ->
                    ""
    in
    { title = "TWHN"
    , body =
        [ Html.toUnstyled <|
            Ui.layout
                { title = title
                , children = [ viewBody model ]
                , route = route
                }
        ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model.status of
        Loaded thread ->
            viewThread thread

        Loading ->
            Ui.centralMessage "Loading..."

        NotFound ->
            Ui.centralMessage "Not found"


viewThread : Thread -> Html msg
viewThread thread =
    let
        renderItem : Item -> Html msg
        renderItem item =
            case item of
                Item__Story story ->
                    viewStory story

                Item__Comment comment ->
                    viewComment comment

                _ ->
                    text ""

        labelToHtml : Item -> Html msg
        labelToHtml item =
            div
                [ css [ p_4, bg_gray_50, rounded_lg, m_4, border_l_2 ]
                ]
                [ renderItem item
                ]

        toListItems : Html msg -> List (Html msg) -> Html msg
        toListItems label children =
            case children of
                [] ->
                    label

                _ ->
                    div []
                        [ label
                        , div
                            [ css [ ml_4 ]
                            ]
                            children
                        ]
    in
    div [ css [ text_sm ] ]
        [ div
            [ css [ p_4, border_b ]
            ]
            [ renderItem (Tree.label thread) ]
        , div []
            (thread
                |> Tree.children
                |> List.map (Tree.restructure labelToHtml toListItems)
            )
        ]


viewStory : Story -> Html msg
viewStory story =
    div [ css [ text_sm ] ]
        [ h1 [ css [ font_bold ] ] [ story.title |> text ]
        , div
            [ css [ flex, items_center, flex_wrap, text_xs, text_gray_500 ] ]
            [ story.url
                |> Maybe.map
                    (\url ->
                        span [ css [ flex, items_center ] ]
                            [ span [ css [ mr_1 ] ] [ viewUrl url ]
                            ]
                    )
                |> Maybe.withDefault (text "")
            , span [ css [ mr_1 ] ] [ text story.humanTime ]
            , span [ css [ mr_1 ] ] [ text "by" ]
            , span [ css [ mr_1 ] ] [ text story.by ]
            , if not (List.isEmpty story.kids) then
                span [ css [ flex, items_center ] ]
                    [ span [ css [ mr_1 ] ]
                        [ (List.length story.kids |> String.fromInt)
                            ++ " comments"
                            |> text
                        ]
                    ]

              else
                text ""
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml story.safeText)
        ]


viewComment : Comment -> Html msg
viewComment comment =
    div []
        [ div [ css [ flex, items_center, pb_2 ] ]
            [ h2 [ css [ font_bold, mr_1 ] ] [ text comment.by ]
            , span [ css [ mr_1 ] ] [ text comment.humanTime ]
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml comment.text)
        ]


viewUrl : Url -> Html msg
viewUrl url =
    a
        [ href (url |> Url.toString)
        , css
            [ block
            , text_gray_500
            , font_light
            , underline
            ]
        ]
        [ url.host |> text
        ]
