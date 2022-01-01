module Ui exposing (centralMessage, layout, viewLink)

import Css
import Css.Global
import Gen.Route as Route exposing (Route)
import Heroicons.Outline
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Svg.Attributes
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)


viewLink : String -> Route -> Html msg
viewLink label route =
    Html.a [ Attr.href (Route.toHref route) ] [ Html.text label ]


viewIconLink : Html msg -> Route -> Html msg
viewIconLink innerHtml route =
    Html.a [ Attr.href (Route.toHref route) ] [ innerHtml ]


centralMessage : String -> Html msg
centralMessage message =
    div
        [ css
            [ pt_4
            , px_2
            , pl_3
            , text_sm
            , flex
            , justify_center
            , h_56
            , items_center
            ]
        ]
        [ text message ]


layout :
    { title : String
    , children : List (Html msg)
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
                    [ css [ mx_auto, fixed ] ]
                    [ viewMainMenu ]
                , div
                    [ css
                        [ Tw.w_full
                        , Breakpoints.lg
                            [ max_w_3xl
                            ]
                        , mx_auto
                        , flex_grow
                        ]
                    ]
                    [ div
                        [ css
                            [ p_4
                            , flex
                            , sticky
                            , top_0
                            , items_center
                            , bg_white
                            , border_l
                            , border_r
                            ]
                        ]
                        [ a
                            [ Attr.href (Route.toHref Route.Home_)
                            , css
                                [ w_6
                                , block
                                , mr_2
                                , Breakpoints.lg
                                    [ Tw.hidden
                                    ]
                                ]
                            ]
                            [ img [ css [ rounded_full ], src "/logo.png" ] [] ]
                        , h1
                            [ css [ font_bold, text_xl ]
                            ]
                            [ text config.title ]
                        ]
                    , div [ css [ border, border_t, border_gray_200 ] ] config.children
                    ]
                ]
            , div
                [ css [ mx_auto, fixed, Tw.hidden, Breakpoints.lg [ block ] ] ]
                [ viewSidebar ]
            ]
        ]


viewMainMenu : Html msg
viewMainMenu =
    div
        [ css [ p_4 ] ]
        [ a
            [ href (Route.toHref Route.Home_)
            , css
                [ mb_8
                , Tw.hidden
                , Breakpoints.lg
                    [ block
                    ]
                ]
            ]
            [ div [ css [ border_0, w_8, h_8, ml_4 ] ] [ img [ css [ rounded_full ], src "/logo.png" ] [] ]
            ]
        , mainMenuLink Route.Home_ "Home" Heroicons.Outline.home
        , mainMenuLink Route.Ask "Ask" Heroicons.Outline.users
        , mainMenuLink Route.Show "Show" Heroicons.Outline.globe
        , mainMenuLink Route.Jobs "Jobs" Heroicons.Outline.briefcase

        --, mainMenuLink Route.Home_ "Stats" Heroicons.Outline.presentationChartLine
        ]


mainMenuLink route val icon =
    a
        [ href (Route.toHref route)
        , css
            [ block
            , rounded_full
            , py_2
            , px_4
            , mt_3
            , Css.hover [ bg_gray_100 ]
            , Tw.hidden
            , Breakpoints.lg
                [ block
                ]
            ]
        ]
        [ div [ css [ flex, items_center ] ]
            [ div [ css [ w_8, h_8, mr_2 ] ] [ icon [] |> Html.fromUnstyled ]
            , div
                [ css
                    [ Tw.hidden
                    , Breakpoints.xl
                        [ block
                        ]
                    ]
                ]
                [ text val ]
            ]
        ]


viewSidebar : Html msg
viewSidebar =
    div []
        [ text ""
        ]
