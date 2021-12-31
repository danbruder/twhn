module Pages.Items.Id_ exposing (Model, Msg, page)

import Api
import Browser.Dom as Dom
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
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Task
import Ui
import Url exposing (Url)
import Util
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req.params.id
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Loading
    | NotFound
    | Loaded
        { item : Item
        , items : Dict Int Item
        }


init : Shared.Model -> String -> ( Model, Effect Msg )
init shared idStr =
    case String.toInt idStr of
        Just id ->
            case Dict.get id shared.items of
                Just item ->
                    let
                        items =
                            Item.kids item
                                |> List.filterMap
                                    (\kid ->
                                        Dict.get kid shared.items
                                            |> Maybe.map (Tuple.pair kid)
                                    )
                                |> Dict.fromList
                    in
                    ( Loaded { item = item, items = items }
                    , [ boot id, resetViewport ]
                        |> Effect.batch
                    )

                Nothing ->
                    ( Loading
                    , boot id
                    )

        _ ->
            ( NotFound, Effect.none )


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
                            ( Loaded
                                { item = item
                                , items = items
                                }
                            , Shared.gotItems (item :: Dict.values items) |> Effect.fromShared
                            )

                        _ ->
                            ( NotFound, Effect.none )

                Err _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View msg
view model =
    let
        title =
            case model of
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
                }
        ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model of
        Loaded { item, items } ->
            viewItem item items

        Loading ->
            Ui.centralMessage "Loading..."

        NotFound ->
            Ui.centralMessage "Not found"


sectionCss =
    [ p_4
    , border_b
    , border_gray_200
    ]


viewItem : Item -> Dict Int Item -> Html msg
viewItem item items =
    let
        getKids : Item -> List Item
        getKids parent =
            parent
                |> Item.kids
                |> List.filterMap (\k -> Dict.get k items)

        renderedTopItem =
            case item of
                Item__Story story ->
                    viewStory story

                Item__Comment comment ->
                    viewComment comment []

                Item__Job job ->
                    div [] []

        renderedComments =
            getKids item
                |> List.filterMap Item.comment
                |> List.map
                    (\c ->
                        viewComment c
                            (getKids (Item__Comment c)
                                |> List.filterMap Item.comment
                            )
                    )
                |> div []
    in
    div []
        [ renderedTopItem
        , renderedComments
        ]


viewStory : Story -> Html msg
viewStory story =
    div [ css [ text_sm ] ]
        [ div
            [ css sectionCss
            ]
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
            , div [] [ story.safeText |> text ]
            ]
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


viewComment : Comment -> List Comment -> Html msg
viewComment comment comments =
    let
        renderSingle inner =
            div []
                [ div [ css [ flex, items_center, pb_2 ] ]
                    [ h2 [ css [ font_bold, mr_1 ] ] [ text inner.by ]
                    , span [ css [ mr_1 ] ] [ text inner.humanTime ]
                    ]
                , div
                    [ class "rendered-comment" ]
                    (Util.textHtml inner.text)
                ]

        thing =
            div [ css [ h_6, w_px, ml_2, bg_gray_300, mt_2, mb_1 ] ] []

        renderSubComment inner =
            div [ css [ my_4, p_4, rounded_lg, bg_gray_50 ] ]
                [ renderSingle inner
                ]

        hasMore =
            List.length comments > 2
    in
    div [ css (sectionCss ++ [ text_sm ]) ]
        [ renderSingle comment
        , div [] <| List.map renderSubComment (List.take 2 comments)
        , if hasMore then
            div [ css [ underline ] ]
                [ Ui.viewLink ("Show this thread (" ++ String.fromInt (List.length comments) ++ ")")
                    (Route.Items__Id_
                        { id = String.fromInt comment.id
                        }
                    )
                ]

          else
            text ""
        ]
