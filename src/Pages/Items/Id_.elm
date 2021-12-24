module Pages.Items.Id_ exposing (Model, Msg, page)

import Api
import Domain exposing (Comment, Story, StoryWithComments)
import Gen.Params.Items.Id_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Juniper.Object.Comment as Comment
import Juniper.Object.Story as Story
import Juniper.Query as Query
import Page
import Request
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import Ui
import Url
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params.id
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { story : Maybe StoryWithComments }


init : String -> ( Model, Cmd Msg )
init idStr =
    case String.toInt idStr of
        Just id ->
            ( { story = Nothing }, getStory id )

        Nothing ->
            ( { story = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = GotStory (Result (Graphql.Http.Error (Maybe StoryWithComments)) (Maybe StoryWithComments))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStory response ->
            case response of
                Ok story ->
                    ( { model | story = story }, Cmd.none )

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
                { title = "Item"
                , children =
                    [ viewStory model ]
                , backRoute = Just Route.Home_
                }
        ]
    }


viewStory : Model -> Html msg
viewStory model =
    case model.story of
        Just story ->
            div [ css [ py_2, text_sm, flex ] ]
                [ div []
                    [ a
                        [ css
                            [ font_bold
                            , mr_2
                            ]
                        ]
                        [ Ui.viewLink story.title
                            (Route.Items__Id_
                                { id =
                                    String.fromInt story.id
                                }
                            )
                        ]
                    , case story.url of
                        Just url ->
                            a
                                [ href (story.url |> Maybe.map Url.toString |> Maybe.withDefault "")
                                , css [ block, text_gray_500, font_light, underline ]
                                ]
                                [ url.host |> text
                                ]

                        Nothing ->
                            span [] []
                    , div []
                        [ ul [] <|
                            (story.comments
                                |> List.map viewComment
                                |> List.map
                                    (\a ->
                                        li
                                            [ css
                                                [ border_t
                                                , border_gray_200
                                                , py_2
                                                ]
                                            ]
                                            [ a ]
                                    )
                            )
                        ]
                    ]
                ]

        Nothing ->
            div [] []


viewComment : Comment -> Html msg
viewComment comment =
    div [ css [] ]
        [ div []
            [ text comment.text
            ]
        ]


getStory : Int -> Cmd Msg
getStory id =
    let
        storySelection =
            SelectionSet.succeed Story
                |> with Story.id
                |> with Story.title
                |> with
                    (SelectionSet.map
                        (Maybe.withDefault ""
                            >> Url.fromString
                        )
                        Story.url
                    )
    in
    Query.storyById
        { id = id }
        (SelectionSet.succeed StoryWithComments
            |> with Story.id
            |> with Story.title
            |> with
                (SelectionSet.map
                    (Maybe.withDefault ""
                        >> Url.fromString
                    )
                    Story.url
                )
            |> with
                (Story.comments
                    (SelectionSet.succeed Comment
                        |> with Comment.id
                        |> with Comment.text
                    )
                )
        )
        |> Api.makeRequest GotStory
