module Ui exposing (layout, viewLink)

import Css
import Css.Global
import Gen.Route as Route exposing (Route)
import Heroicons.Outline
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)


viewLink : String -> Route -> Html msg
viewLink label route =
    Html.a [ Attr.href (Route.toHref route) ] [ Html.text label ]


viewIconLink : Html msg -> Route -> Html msg
viewIconLink innerHtml route =
    Html.a [ Attr.href (Route.toHref route) ] [ innerHtml ]


layout :
    { title : String
    , children : List (Html msg)
    , backRoute : Maybe Route
    }
    -> Html msg
layout config =
    div []
        [ div
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
                    [ div
                        [ css
                            [ p_4
                            , border_t
                            , border_r
                            , border_l
                            , border_gray_200
                            , flex
                            , items_center
                            ]
                        ]
                        [ case config.backRoute of
                            Just route ->
                                div
                                    [ css
                                        [ w_5
                                        , mr_2
                                        ]
                                    ]
                                    [ viewIconLink
                                        (Heroicons.Outline.arrowCircleLeft [] |> Html.fromUnstyled)
                                        route
                                    ]

                            Nothing ->
                                text ""
                        , h1
                            [ css [ font_bold, text_xl ]
                            ]
                            [ text config.title ]
                        ]
                    , div [ css [ p_4, border, border_gray_200 ] ] config.children
                    ]
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
