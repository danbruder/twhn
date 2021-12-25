module Pages.Stories.Id_ exposing (Model, Msg, page)

import Api
import Domain exposing (..)
import Gen.Params.Stories.Id_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
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
import Url exposing (Url)
import Util
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
    { story : Maybe Story }


init : String -> ( Model, Cmd Msg )
init idStr =
    case String.toInt idStr of
        Just id ->
            ( { story = Nothing }, getStory id )

        Nothing ->
            ( { story = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = GotStory (Result (Graphql.Http.Error (Maybe Story)) (Maybe Story))


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
                { title = "Story"
                , children = [ viewBody model ]
                }
        ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model.story of
        Just story ->
            viewStory story

        Nothing ->
            div [] []


sectionCss =
    [ p_4
    , border_b
    , border_gray_200
    ]


viewStory : Story -> Html msg
viewStory story =
    div [ css [ text_sm ] ]
        [ div
            [ css sectionCss
            ]
            [ h1 [ css [ font_bold ] ] [ story.title |> text ]
            , div
                [ css [ flex, items_center, text_xs, text_gray_500 ] ]
                [ story.url
                    |> Maybe.map
                        (\url ->
                            span [ css [ flex, items_center ] ]
                                [ span [ css [ mr_1 ] ] [ viewUrl url ]
                                , span [ css [ mr_1 ] ] [ text "·" ]
                                ]
                        )
                    |> Maybe.withDefault (text "")
                , span [ css [ mr_1 ] ] [ text story.humanTime ]
                , span [ css [ mr_1 ] ] [ text "by" ]
                , span [ css [] ] [ text story.by ]
                ]
            ]
        , div []
            [ ul [] <|
                (story.comments
                    |> List.map viewComment
                    |> List.map
                        (\item ->
                            li
                                [ css sectionCss
                                ]
                                [ item ]
                        )
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
viewComment (Comment comment) =
    div []
        [ div [ css [ flex, items_center, pb_2 ] ]
            [ h2 [ css [ font_bold, mr_1 ] ]
                [ text comment.by ]
            , span [ css [] ]
                [ Ui.viewLink comment.humanTime (Route.Comments__Id_ { id = String.fromInt comment.id })
                ]
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml comment.text)
        ]


getStory : Int -> Cmd Msg
getStory id =
    Query.storyById
        { id = id }
        (SelectionSet.succeed Story
            |> with Story.id
            |> with Story.title
            |> with (SelectionSet.map (Maybe.withDefault "" >> Url.fromString) Story.url)
            |> with
                (Story.comments
                    (SelectionSet.succeed newComment
                        |> with Comment.id
                        |> with Comment.safeText
                        |> with Comment.by
                        |> with Comment.humanTime
                        |> hardcoded []
                        |> with Comment.parent
                    )
                )
            |> with Story.by
            |> with Story.score
            |> with Story.humanTime
        )
        |> Api.makeRequest GotStory
