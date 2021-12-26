module Pages.Items.Id_ exposing (Model, Msg, page)

import Api
import Dict
import Domain.Comment exposing (Comment)
import Domain.Item exposing (Item(..))
import Domain.Story exposing (Story)
import Effect exposing (Effect)
import Gen.Params.Comments.Id_ exposing (Params)
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
        }


init : Shared.Model -> String -> ( Model, Effect Msg )
init shared idStr =
    case String.toInt idStr of
        Just id ->
            case Dict.get id shared.items of
                Just item ->
                    ( Loaded { item = item }
                    , Query.itemById { id = id } Selections.item
                        |> Api.makeRequest GotItem
                        |> Effect.fromCmd
                    )

                Nothing ->
                    ( Loading
                    , Query.itemById { id = id } Selections.item
                        |> Api.makeRequest GotItem
                        |> Effect.fromCmd
                    )

        _ ->
            ( NotFound, Effect.none )



-- UPDATE


type Msg
    = GotItem (Result (Graphql.Http.Error (Maybe Item)) (Maybe Item))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotItem response ->
            case response of
                Ok (Just item) ->
                    ( Loaded { item = item }, Effect.none )

                Ok Nothing ->
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
        Loading ->
            div [] [ text "loading..." ]

        Loaded { item } ->
            viewItem item

        NotFound ->
            div [] [ text "not found" ]


sectionCss =
    [ p_4
    , border_b
    , border_gray_200
    ]


viewItem : Item -> Html msg
viewItem item =
    case item of
        Item__Story story ->
            viewStory story

        Item__Comment comment ->
            viewComment comment


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
            ]
        , div []
            [ ul [] <|
                -- (story.comments
                --     |> List.map viewComment
                --     |> List.map
                --         (\item ->
                --             li
                --                 [ css sectionCss
                --                 ]
                --                 [ item ]
                --         )
                -- )
                []
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
                        (Route.Items__Id_ { id = String.fromInt comment.id })
                    ]

              else
                text ""
            ]
        , div
            [ class "rendered-comment" ]
            (Util.textHtml comment.text)
        ]
