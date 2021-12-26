module Pages.Home_ exposing (Model, Msg, page)

import Api
import Css
import Css.Global
import Dict exposing (Dict)
import Domain.Item as Item exposing (Item(..))
import Domain.Story exposing (Story)
import Effect exposing (Effect)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Http
import Juniper.Object.Story as Story
import Juniper.Query as Query
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


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Loading
    | Loaded (List Item)
    | NotFound


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    case
        shared.topItems
            |> List.filterMap (\id -> Dict.get id shared.items)
    of
        [] ->
            ( Loading, getTopItems )

        items ->
            ( Loaded items, getTopItems )


getTopItems : Effect Msg
getTopItems =
    Query.topItems
        (\optionals -> { optionals | limit = Present 30 })
        Selections.item
        |> Api.makeRequest GotTopItems
        |> Effect.fromCmd



-- UPDATE


type Msg
    = GotTopItems (Result (Graphql.Http.Error (List Item)) (List Item))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotTopItems response ->
            case response of
                Ok items ->
                    ( Loaded items
                    , [ Shared.gotItems items |> Effect.fromShared
                      , Shared.gotTopItems (List.map Item.id items) |> Effect.fromShared
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


view : Model -> View msg
view model =
    { title = "TWHN"
    , body =
        [ Html.toUnstyled <|
            Ui.layout
                { title = "Home"
                , children =
                    [ case model of
                        Loading ->
                            text ""

                        Loaded items ->
                            viewItems items

                        NotFound ->
                            text "Error"
                    ]
                }
        ]
    }


viewItems : List Item -> Html msg
viewItems items =
    items
        |> List.filterMap
            (\i ->
                case i of
                    Item__Story story ->
                        Just story

                    _ ->
                        Nothing
            )
        |> List.indexedMap viewStory
        |> ul []


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
