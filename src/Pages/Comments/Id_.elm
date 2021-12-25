module Pages.Comments.Id_ exposing (Model, Msg, page)

import Api
import Domain exposing (..)
import Gen.Params.Comments.Id_ exposing (Params)
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
    { comment : Maybe Comment }


init : String -> ( Model, Cmd Msg )
init idStr =
    case String.toInt idStr of
        Just id ->
            ( { comment = Nothing }, getComment id )

        Nothing ->
            ( { comment = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = GotComment (Result (Graphql.Http.Error (Maybe Comment)) (Maybe Comment))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotComment response ->
            case response of
                Ok comment ->
                    ( { model | comment = comment }, Cmd.none )

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
                { title = "Comment"
                , children = [ viewBody model ]
                }
        ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model.comment of
        Just comment ->
            viewComment comment

        Nothing ->
            div [] []


sectionCss =
    [ p_4
    , border_b
    , border_gray_200
    ]


viewComment : Comment -> Html msg
viewComment (Comment comment) =
    div [ css [ text_sm ] ]
        [ div [ css sectionCss ]
            [ viewSubComment (Comment comment)
            ]
        , div []
            [ ul [] <|
                (comment.comments
                    |> List.map viewSubComment
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


viewSubComment : Comment -> Html msg
viewSubComment (Comment comment) =
    div []
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
                        (Route.Comments__Id_ { id = String.fromInt comment.id })
                    ]

              else
                text ""
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml comment.text)
        ]


getComment : Int -> Cmd Msg
getComment id =
    let
        selectionSet =
            SelectionSet.succeed newComment
                |> with Comment.id
                |> with Comment.safeText
                |> with Comment.by
                |> with Comment.humanTime
                |> hardcoded []
                |> with Comment.parent
                |> with (SelectionSet.withDefault [] Comment.kids)
    in
    Query.commentById
        { id = id }
        (SelectionSet.succeed newComment
            |> with Comment.id
            |> with Comment.safeText
            |> with Comment.by
            |> with Comment.humanTime
            |> with (Comment.comments selectionSet)
            |> with Comment.parent
            |> with (SelectionSet.withDefault [] Comment.kids)
        )
        |> Api.makeRequest GotComment
