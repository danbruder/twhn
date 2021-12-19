module Pages.Home_ exposing (Model, Msg, page)

import Api
import Css
import Css.Global
import Dict exposing (Dict)
import Gen.Params.Home_ exposing (Params)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Http
import Json.Decode as JD
import Juniper.Object.Story as Story
import Juniper.Query as Query
import Page
import Request
import Set exposing (Set)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
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
            body model
        ]
    }


body : Model -> Html msg
body model =
    div
        [ css
            [ max_w_7xl
            , mx_auto
            , Breakpoints.lg
                [ px_8
                ]
            ]
        ]
        [ Css.Global.global Tw.globalStyles
        , div [ css [ flex ] ]
            [ div
                [ css [ mx_auto, Tw.hidden, Breakpoints.lg [ block ] ] ]
                [ viewMainMenu ]
            , div
                [ css [ max_w_4xl, mx_auto, flex_grow ] ]
                [ div [ css [ p_4, border_t, border_r, border_l, border_gray_200 ] ] [ h1 [ css [ font_bold, text_xl ] ] [ text "Home" ] ]
                , div [ css [ p_4, border, border_gray_200 ] ] [ viewStories model ]
                ]
            , div
                [ css [ mx_auto, Tw.hidden, Breakpoints.lg [ block ] ] ]
                [ viewSidebar ]
            ]
        ]


viewMainMenu : Html msg
viewMainMenu =
    div
        [ css
            [ p_4
            ]
        ]
        [ div
            [ css
                [ border_0
                , w_8
                , h_8
                ]
            ]
            [ img [ css [ rounded_full ], src "/logo.png" ] []
            ]
        ]


viewSidebar : Html msg
viewSidebar =
    div []
        [ text ""
        ]


viewStories : Model -> Html msg
viewStories model =
    ul
        [ css []
        ]
    <|
        (model.stories
            |> List.indexedMap
                (\i story ->
                    li [ css [ py_2, text_sm ] ]
                        [ a
                            [ css
                                [ font_bold
                                , mr_2
                                , if story.url == Nothing then
                                    block

                                  else
                                    underline
                                ]
                            , href (story.url |> Maybe.map Url.toString |> Maybe.withDefault "")
                            , target "blank"
                            ]
                            [ [ String.fromInt (i + 1)
                              , "."
                              , " "
                              , story.title
                              ]
                                |> String.join ""
                                |> text
                            ]
                        , case story.url of
                            Just url ->
                                span [ css [ text_gray_500, font_light ] ] [ [ "(", url.host, ")" ] |> String.join "" |> text ]

                            Nothing ->
                                span [] []
                        ]
                )
        )


type alias Story =
    { id : Int
    , title : String
    , url : Maybe Url
    }



-- HTTP


getTopStories : Cmd Msg
getTopStories =
    Query.topStories identity
        (SelectionSet.succeed Story
            |> with Story.id
            |> with Story.title
            |> with
                (SelectionSet.map
                    (Maybe.withDefault ""
                        >> Url.fromString
                    )
                    Story.url
                )
        )
        |> Api.makeRequest GotTopStories
