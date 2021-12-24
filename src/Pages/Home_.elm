module Pages.Home_ exposing (Model, Msg, page)

import Api
import Css
import Css.Global
import Dict exposing (Dict)
import Domain exposing (Story)
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
import Set exposing (Set)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Ui
import Url exposing (Url)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { stories : List Story
    }


init : ( Model, Cmd Msg )
init =
    ( { stories = [] }, getTopStories )



-- UPDATE


type Msg
    = GotTopStories (Result (Graphql.Http.Error (List Story)) (List Story))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTopStories response ->
            case response of
                Ok stories ->
                    ( { model | stories = stories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



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
                    [ viewStories model ]
                , backRoute = Nothing
                }
        ]
    }


viewStories : Model -> Html msg
viewStories model =
    ul
        [ css []
        ]
    <|
        (model.stories
            |> List.indexedMap
                (\i story ->
                    li [ css [ pt_4, px_2, pl_3, text_sm, flex ] ]
                        [ span [ css [ w_6, flex_shrink_0, text_gray_500 ] ] [ [ String.fromInt (i + 1), ".", " " ] |> String.join "" |> text ]
                        , div []
                            [ div [ css [ font_bold, mr_2 ] ]
                                [ Ui.viewLink story.title (Route.Items__Id_ { id = String.fromInt story.id })
                                ]
                            , div [ css [ flex, items_center, text_xs, text_gray_500 ] ]
                                [ span [ css [ mr_1 ] ] [ story.url |> Maybe.map viewUrl |> Maybe.withDefault (text "") ]
                                , span [ css [ mr_1 ] ] [ text "·" ]
                                , span [ css [ mr_1 ] ] [ text story.humanTime ]
                                , span [ css [ mr_1 ] ] [ text "by" ]
                                , span [ css [ mr_1 ] ] [ story.by ]
                                ]
                            ]
                        ]
                )
        )


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



-- HTTP


getTopStories : Cmd Msg
getTopStories =
    Query.topStories
        (\optionals ->
            { optionals
                | limit = Present 30
            }
        )
        (SelectionSet.succeed Story
            |> with Story.id
            |> with Story.title
            |> with (SelectionSet.map (Maybe.withDefault "" >> Url.fromString) Story.url)
            |> hardcoded []
            |> with Story.by
            |> with Story.score
            |> with Story.humanTime
        )
        |> Api.makeRequest GotTopStories
