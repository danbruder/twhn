module Components.List exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Css
import Css.Global
import Dict exposing (Dict)
import Domain.Item as Item exposing (Item(..))
import Domain.Job exposing (Job)
import Domain.Story exposing (Story)
import Effect exposing (Effect)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument as OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Http
import Juniper.Object.Stats as Stats
import Juniper.Object.Story as Story
import Juniper.Query as Query
import Juniper.Union
import Page
import Request
import Selections
import Set exposing (Set)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Ui
import Url exposing (Url)
import View exposing (View)



-- INIT


type Model
    = Loading
    | Loaded (List Item)
    | NotFound


init :
    { cachedList : List Int
    , allItems : Dict Int Item
    , query : ItemsQuery Item
    }
    -> ( Model, Effect Msg )
init { cachedList, query, allItems } =
    case
        cachedList
            |> List.filterMap (\id -> Dict.get id allItems)
    of
        [] ->
            ( Loading, getItems query )

        items ->
            ( Loaded items, getItems query )


type alias ItemsQuery decodesTo =
    ({ limit : OptionalArgument Int } -> { limit : OptionalArgument Int })
    -> SelectionSet decodesTo Juniper.Union.Item
    -> SelectionSet (List decodesTo) RootQuery


getItems : ItemsQuery Item -> Effect Msg
getItems query =
    query
        (\optionals -> { optionals | limit = Present 30 })
        Selections.item
        |> Api.makeRequest GotTopItems
        |> Effect.fromCmd


getItemKids : ItemsQuery (List Item) -> Effect Msg
getItemKids query =
    query
        (\optionals -> { optionals | limit = Present 30 })
        Selections.children
        |> Api.makeRequest GotTopChildren
        |> Effect.fromCmd



-- UPDATE


type Msg
    = GotTopItems (Result (Graphql.Http.Error (List Item)) (List Item))
    | GotTopChildren (Result (Graphql.Http.Error (List (List Item))) (List (List Item)))


update : String -> ItemsQuery (List Item) -> Msg -> Model -> ( Model, Effect Msg )
update key query msg model =
    case msg of
        GotTopItems response ->
            case response of
                Ok items ->
                    ( Loaded items
                    , [ Shared.gotItems items |> Effect.fromShared
                      , Shared.gotListIndex key (List.map Item.id items) |> Effect.fromShared
                      , getItemKids query
                      ]
                        |> Effect.batch
                    )

                Err _ ->
                    ( model, Effect.none )

        GotTopChildren response ->
            case response of
                Ok items ->
                    ( model
                    , [ Shared.gotItems (List.concat items) |> Effect.fromShared
                      ]
                        |> Effect.batch
                    )

                Err _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : String -> Route -> Model -> View msg
view title route model =
    { title = "TWHN"
    , body =
        [ Html.toUnstyled <|
            Ui.layout
                { title = title
                , children =
                    [ case model of
                        Loading ->
                            Ui.centralMessage "Loading..."

                        Loaded items ->
                            viewItems items

                        NotFound ->
                            Ui.centralMessage "Something went wrong..."
                    ]
                , route = route
                }
        ]
    }


viewItems : List Item -> Html msg
viewItems items =
    if List.isEmpty items then
        div
            [ css
                [ text_sm
                , text_gray_500
                , flex
                , justify_center
                , items_center
                , py_8
                ]
            ]
            [ text "Nothing here..." ]

    else
        items
            |> List.filter (\item -> Item.isStory item || Item.isJob item)
            |> List.indexedMap
                (\index item ->
                    case item of
                        Item__Story story ->
                            viewStory index story

                        Item__Job job ->
                            viewJob index job

                        _ ->
                            text ""
                )
            |> ul
                [ css [ pb_4 ]
                ]


viewStory : Int -> Story -> Html msg
viewStory index story =
    li [ css [ pt_4, px_2, pl_3, text_sm, flex ] ]
        [ span [ css [ w_6, flex_shrink_0, text_gray_500 ] ] [ [ String.fromInt (index + 1), ".", " " ] |> String.join "" |> text ]
        , div []
            [ div [ css [ font_bold, mr_2 ] ]
                [ Ui.viewLink story.title (Route.Items__Id_ { id = String.fromInt story.id })
                ]
            , div
                [ css
                    [ flex
                    , items_center
                    , flex_wrap
                    , text_xs
                    , text_gray_500
                    ]
                ]
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
                ]
            ]
        ]


viewJob : Int -> Job -> Html msg
viewJob index job =
    li [ css [ pt_4, px_2, pl_3, text_sm, flex ] ]
        [ span [ css [ w_6, flex_shrink_0, text_gray_500 ] ] [ [ String.fromInt (index + 1), ".", " " ] |> String.join "" |> text ]
        , div []
            [ div [ css [ font_bold, mr_2 ] ]
                [ job.url
                    |> Maybe.map
                        (\url ->
                            a [ href (Url.toString url), target "blank_" ] [ text job.title ]
                        )
                    |> Maybe.withDefault (text job.title)
                ]
            , div
                [ css
                    [ flex
                    , items_center
                    , flex_wrap
                    , text_xs
                    , text_gray_500
                    ]
                ]
                [ span [ css [ mr_1 ] ] [ text job.humanTime ]
                ]
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
