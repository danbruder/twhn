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
        , children : Dict Int Item
        }


init : Shared.Model -> String -> ( Model, Effect Msg )
init shared idStr =
    case String.toInt idStr of
        Just id ->
            case Dict.get id shared.items of
                Just item ->
                    let
                        children =
                            Item.kids item
                                |> List.filterMap
                                    (\kid ->
                                        Dict.get kid shared.items
                                            |> Maybe.map (Tuple.pair kid)
                                    )
                                |> Dict.fromList
                    in
                    ( Loaded { item = item, children = children }
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
    }


boot : Int -> Effect Msg
boot id =
    SelectionSet.map2
        (\maybeItem maybeChildren ->
            { item = maybeItem
            , children = maybeChildren
            }
        )
        (Query.itemById { id = id } Selections.item)
        (Query.itemById { id = id } Selections.children)
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
                    case ( payload.item, payload.children ) of
                        ( Just item, Just children ) ->
                            ( Loaded
                                { item = item
                                , children =
                                    children
                                        |> List.map (\item_ -> ( Item.id item_, item_ ))
                                        |> Dict.fromList
                                }
                            , Shared.gotItems (item :: children) |> Effect.fromShared
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
    { title = "TWHN"
    , body =
        [ Html.toUnstyled <|
            Ui.layout
                { title = "Story"
                , children = [ viewBody model ]
                }
        ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model of
        Loaded { item, children } ->
            viewItem item
                (Item.kids item |> List.filterMap (\k -> Dict.get k children))

        Loading ->
            Ui.centralMessage "Loading..."

        NotFound ->
            Ui.centralMessage "Not found"


sectionCss =
    [ p_4
    , border_b
    , border_gray_200
    ]


viewItem : Item -> List Item -> Html msg
viewItem item children =
    case item of
        Item__Story story ->
            viewStory story
                (children
                    |> List.filterMap Item.comment
                    |> List.filter
                        (\comment ->
                            comment.parent == story.id
                        )
                )

        Item__Comment comment ->
            comment
                :: (children
                        |> List.filterMap Item.comment
                        |> List.filter
                            (\c ->
                                c.parent == comment.id
                            )
                   )
                |> List.map viewComment
                |> div []


viewStory : Story -> List Comment -> Html msg
viewStory story comments =
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
            ]
        , div []
            [ ul [] <|
                (comments
                    |> List.map viewComment
                )
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


viewComment : Comment -> Html msg
viewComment comment =
    div [ css (sectionCss ++ [ text_sm ]) ]
        [ div [ css [ flex, items_center, pb_2 ] ]
            [ h2 [ css [ font_bold, mr_1 ] ] [ text comment.by ]
            , span [ css [ mr_1 ] ] [ text comment.humanTime ]
            , if not (List.isEmpty comment.kids) then
                span [ css [ underline, text_gray_500 ] ]
                    [ Ui.viewLink
                        (case List.length comment.kids of
                            1 ->
                                "(1 reply)"

                            a ->
                                "(" ++ String.fromInt a ++ " replies)"
                        )
                        (Route.Items__Id_ { id = String.fromInt comment.id })
                    ]

              else
                text ""
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml comment.text)
        ]
