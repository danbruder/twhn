module Ui exposing (centralMessage, layout, viewLink, viewLinkWithQuery)

import Css
import Css.Global
import Dict exposing (Dict)
import Gen.Route as Route exposing (Route)
import Heroicons.Outline
import Heroicons.Solid
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Svg.Attributes
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)


viewLink : String -> Route -> Html msg
viewLink label route =
    Html.a [ Attr.href (Route.toHref route) ] [ Html.text label ]


viewLinkWithQuery : String -> Route -> Dict String String -> Html msg
viewLinkWithQuery label route query =
    let
        queryStr =
            Dict.toList query
                |> List.map
                    (\( key, val ) ->
                        key ++ "=" ++ val
                    )
                |> String.join "&"

        finalRoute =
            [ Route.toHref route
            , "?"
            , queryStr
            ]
                |> String.join ""
    in
    Html.a
        [ Attr.href finalRoute
        ]
        [ Html.text label ]


viewIconLink : Html msg -> Route -> Html msg
viewIconLink innerHtml route =
    Html.a [ Attr.href (Route.toHref route) ] [ innerHtml ]


centralMessage : String -> Html msg
centralMessage message =
    div
        [ css
            [ px_2
            , pl_3
            , text_sm
            , flex
            , justify_center
            , h_56
            , pt_16
            , h_screen
            ]
        ]
        [ text message ]


layout :
    { title : String
    , children : List (Html msg)
    , route : Route
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
                [ div [ css [ sticky, top_0, bottom_0, h_screen ] ] [ viewMainMenu config.route ]
                , viewMobileMenu config.route
                , div [ css [ Tw.w_full, Breakpoints.lg [ max_w_3xl ], flex_grow ] ]
                    [ div [ css [ p_4, flex, sticky, top_0, items_center, bg_white, border_l, border_r, z_10 ] ]
                        [ a [ Attr.href (Route.toHref Route.Home_), css [ w_6, block, mr_2, Breakpoints.lg [ Tw.hidden ] ] ] [ img [ css [ rounded_full ], src "/logo.png" ] [] ]
                        , h1 [ css [ font_bold, text_xl ] ] [ text config.title ]
                        ]
                    , div [ css [ border, border_t, border_gray_200, pb_32, Breakpoints.lg [ pb_16 ] ], class "scrollbar-none" ] config.children
                    ]
                ]
            , div
                [ css [ mx_auto, Tw.hidden, Breakpoints.lg [ block ] ] ]
                []
            ]
        ]


viewMainMenu : Route -> Html msg
viewMainMenu currentRoute =
    let
        isActive route =
            route == currentRoute

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
                    , if isActive route then
                        font_bold

                      else
                        font_normal
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
    in
    div
        [ css
            [ Breakpoints.lg [ p_4 ]
            , Breakpoints.xl [ w_56 ]
            ]
        ]
        [ a
            [ href (Route.toHref Route.Home_), css [ mb_8, Tw.hidden, Breakpoints.lg [ block ] ] ]
            [ div [ css [ border_0, w_8, h_8, ml_4 ] ] [ img [ css [ rounded_full ], src "/logo.png" ] [] ] ]
        , mainMenuLink Route.Home_ "Home" Heroicons.Outline.home
        , mainMenuLink Route.Ask "Ask" Heroicons.Outline.users
        , mainMenuLink Route.Show "Show" Heroicons.Outline.globe
        , mainMenuLink Route.Jobs "Jobs" Heroicons.Outline.briefcase
        , mainMenuLink Route.New "New" Heroicons.Outline.newspaper
        , mainMenuLink Route.Bookmarks "Bookmarks" Heroicons.Outline.bookmark
        ]


viewMobileMenu : Route -> Html msg
viewMobileMenu currentRoute =
    let
        isActive route =
            route == currentRoute

        mainMenuLink route val icon iconSolid =
            a
                [ href (Route.toHref route)
                , css
                    [ block
                    , rounded_full
                    , p_4
                    , block
                    , Breakpoints.lg
                        [ Tw.hidden
                        ]
                    , if isActive route then
                        font_bold

                      else
                        font_normal
                    ]
                ]
                [ div [ css [ flex, items_center ] ]
                    [ div [ css [ w_7, h_7, mr_2 ] ]
                        [ (if isActive route then
                            iconSolid []

                           else
                            icon []
                          )
                            |> Html.fromUnstyled
                        ]
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
    in
    div
        [ css [ fixed, bottom_0, flex, justify_around, w_full, bg_white, border_t ] ]
        [ mainMenuLink Route.Home_ "Home" Heroicons.Outline.home Heroicons.Solid.home
        , mainMenuLink Route.Ask "Ask" Heroicons.Outline.users Heroicons.Solid.users
        , mainMenuLink Route.Show "Show" Heroicons.Outline.globe Heroicons.Solid.globe
        , mainMenuLink Route.Jobs "Jobs" Heroicons.Outline.briefcase Heroicons.Solid.briefcase
        , mainMenuLink Route.New "New" Heroicons.Outline.newspaper Heroicons.Solid.newspaper
        , mainMenuLink Route.Bookmarks "Bookmarks" Heroicons.Outline.bookmark Heroicons.Solid.bookmark
        ]


viewSidebar : Html msg
viewSidebar =
    div []
        [ text ""
        ]
