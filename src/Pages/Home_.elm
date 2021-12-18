module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Global
import Dict exposing (Dict)
import Gen.Params.Home_ exposing (Params)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Http
import Json.Decode as JD
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
    { stories : Dict Int Story
    , topIds : List Int
    , fetched : Set Int
    , toFetch : Set Int
    }


init : ( Model, Cmd Msg )
init =
    ( { stories = Dict.empty, topIds = [], fetched = Set.empty, toFetch = Set.empty }, getTopStories )


storiesPerPage =
    10


totalToFetch =
    50



-- UPDATE


type Msg
    = GotTopStories (Result Http.Error (List Int))
    | GotStory (Result Http.Error Story)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTopStories response ->
            case response of
                Ok storyIds ->
                    let
                        topIds =
                            storyIds |> List.take totalToFetch

                        fetched =
                            topIds
                                |> List.take storiesPerPage

                        toFetch =
                            topIds
                                |> List.drop storiesPerPage
                    in
                    ( { model | topIds = topIds, toFetch = Set.fromList toFetch }
                    , fetched
                        |> List.map getStoryById
                        |> Cmd.batch
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotStory response ->
            case response of
                Ok story ->
                    case model.toFetch |> Set.toList |> List.head of
                        Just toFetch ->
                            ( { model
                                | stories = Dict.insert story.id story model.stories
                                , fetched = Set.insert story.id model.fetched
                                , toFetch = Set.remove toFetch model.toFetch
                              }
                            , getStoryById toFetch
                            )

                        Nothing ->
                            ( { model
                                | stories = Dict.insert story.id story model.stories
                              }
                            , Cmd.none
                            )

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
            , px_4
            , Breakpoints.sm
                [ px_6
                ]
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
        (model.topIds
            |> List.filterMap (\i -> Dict.get i model.stories)
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
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json"
        , expect = Http.expectJson GotTopStories (JD.list JD.int)
        }


getStoryById : Int -> Cmd Msg
getStoryById id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt id ++ ".json"
        , expect = Http.expectJson GotStory storyDecoder
        }


storyDecoder : JD.Decoder Story
storyDecoder =
    JD.map3 Story
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "url"
            (JD.string
                |> JD.andThen
                    (\urlString ->
                        case Url.fromString urlString of
                            Just url ->
                                JD.succeed (Just url)

                            Nothing ->
                                JD.succeed Nothing
                    )
            )
        )
