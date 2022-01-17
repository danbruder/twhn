module Pages.Items.Id_ exposing (Model, Msg, page)

import Api
import Browser.Dom as Dom
import Chart as C
import Chart.Attributes as CA
import Css
import Dict exposing (Dict)
import Domain.Comment as Comment exposing (Comment)
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
import Html.Styled.Events as Ev
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
import Svg as S
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Task
import Time
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


buildThread : Item -> Set Int -> Dict Int Item -> Thread
buildThread root expandedItems items =
    let
        makeTree item =
            if Set.member (Item.id item) expandedItems then
                Tree.tree item (handleKids item)

            else
                Tree.singleton item

        handleKids item =
            Item.kids item
                |> List.filterMap (\id -> Dict.get id items)
                |> List.map makeTree
    in
    Tree.tree root (handleKids root)



-- INIT


type alias Model =
    { status : Status
    , expandedItems : Set Int
    }


type Status
    = Loading
    | NotFound
    | Loaded
        { items : Dict Int Item
        , item : Item
        }


init : Shared.Model -> String -> ( Model, Effect Msg )
init shared idStr =
    case String.toInt idStr of
        Just id ->
            case Dict.get id shared.items of
                Just item ->
                    ( { status =
                            Loaded
                                { items = shared.items
                                , item = item
                                }
                      , expandedItems = Set.empty
                      }
                    , [ boot id, resetViewport ]
                        |> Effect.batch
                    )

                Nothing ->
                    ( { status = Loading
                      , expandedItems = Set.empty
                      }
                    , boot id
                    )

        _ ->
            ( { status = NotFound
              , expandedItems = Set.empty
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
    | ClickedExpandItem Int
    | ClickedCollapseItem Int
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        ClickedExpandItem id ->
            ( { model | expandedItems = Set.insert id model.expandedItems }, Effect.none )

        ClickedCollapseItem id ->
            ( { model | expandedItems = Set.remove id model.expandedItems }, Effect.none )

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
                            ( { model
                                | status =
                                    Loaded
                                        { item = item
                                        , items = items
                                        }
                              }
                            , Shared.gotItems (item :: Dict.values items) |> Effect.fromShared
                            )

                        _ ->
                            ( { model | status = NotFound }, Effect.none )

                Err _ ->
                    ( model, Effect.none )



-- VIEW


view : Route -> Model -> View Msg
view route model =
    let
        title =
            case model.status of
                Loaded { item } ->
                    if Item.isComment item then
                        "Comment"

                    else if Item.isStory item then
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


viewBody : Model -> Html Msg
viewBody model =
    case model.status of
        Loaded { items, item } ->
            viewThread (buildThread item model.expandedItems items) model.expandedItems

        Loading ->
            Ui.centralMessage "Loading..."

        NotFound ->
            Ui.centralMessage "Not found"


viewThread : Thread -> Set Int -> Html Msg
viewThread thread expandedItems =
    let
        renderFirstItem : Item -> Html Msg
        renderFirstItem item =
            case item of
                Item__Story story ->
                    div []
                        [ viewStory story
                        , if not (List.isEmpty story.ranks) then
                            chart story.ranks

                          else
                            text ""
                        ]

                _ ->
                    text ""

        renderItem : Item -> Html Msg
        renderItem item =
            case item of
                Item__Story story ->
                    viewStory story

                Item__Comment comment ->
                    viewComment comment (Set.member comment.id expandedItems)

                _ ->
                    text ""

        labelToHtml : Item -> Html Msg
        labelToHtml item =
            div
                [ css [ p_4, bg_gray_50, rounded_lg, m_4, border_l_2 ]
                ]
                [ renderItem item
                ]

        toListItems : Html Msg -> List (Html Msg) -> Html Msg
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

        chart : List Domain.Story.Rank -> Html Msg
        chart ranks =
            [ C.chart
                [ CA.height 100
                , CA.width 300
                ]
                [ C.xLabels [ CA.times Time.utc ]
                , C.series (.ts >> Time.posixToMillis >> toFloat)
                    [ C.interpolated (.value >> (-) 30 >> toFloat) [ CA.opacity 0.3, CA.gradient [], CA.color "orange" ] []
                    ]
                    ranks
                ]
                |> Html.fromUnstyled
            ]
                |> div [ css [ w_40, h_20, pt_4 ] ]
    in
    div [ css [ text_sm ] ]
        [ div
            [ css [ p_4, border_b, relative ]
            ]
            [ renderFirstItem (Tree.label thread)
            ]
        , div []
            (thread
                |> Tree.children
                |> List.map (Tree.restructure labelToHtml toListItems)
            )
        ]


viewStory : Story -> Html Msg
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


viewComment : Comment -> Bool -> Html Msg
viewComment comment isExpanded =
    let
        childrenCount =
            List.length comment.kids

        hasKids =
            childrenCount > 0

        buttonCss =
            [ border, p_1, Css.cursor Css.pointer ]
    in
    div []
        [ div [ css [ flex, items_center, pb_2 ] ]
            [ h2 [ css [ font_bold, mr_1 ] ] [ text comment.by ]
            , span [ css [ mr_1 ] ] [ text comment.humanTime ]
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml comment.text)
        , div
            [ css
                [ flex
                , justify_end
                , pt_2
                , text_gray_500
                ]
            ]
            [ case ( hasKids, isExpanded ) of
                ( True, True ) ->
                    div
                        [ Ev.onClick (ClickedCollapseItem comment.id)
                        , css buttonCss
                        ]
                        [ text "Collapse Comments"
                        ]

                ( True, False ) ->
                    let
                        plural =
                            childrenCount > 1
                    in
                    div
                        [ Ev.onClick (ClickedExpandItem comment.id)
                        , css buttonCss
                        ]
                        [ text
                            ("Comment"
                                ++ (if plural then
                                        "s"

                                    else
                                        ""
                                   )
                                ++ " ("
                                ++ String.fromInt childrenCount
                                ++ ")"
                            )
                        ]

                ( False, _ ) ->
                    text ""
            ]
        ]


viewUrl : Url -> Html Msg
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
