module Pages.Home_ exposing (view)

import Css
import Css.Global
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw exposing (..)
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        [ Html.toUnstyled <|
            body
        ]
    }


body =
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
                [ css [ mx_auto ] ]
                [ viewMainMenu ]
            , div
                [ css [ max_w_4xl, mx_auto, flex_grow ] ]
                [ div [ css [ p_4, border_t, border_r, border_l, border_gray_200 ] ] [ h1 [ css [ font_bold, text_xl ] ] [ text "Home" ] ]
                , div [ css [ p_4, border, border_gray_200 ] ] [ viewStories ]
                ]
            , div
                [ css [] ]
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
        [ text "Sidebar"
        ]


viewStories : Html msg
viewStories =
    ul
        [ css []
        ]
    <|
        (stories
            |> List.indexedMap
                (\i story ->
                    li [ css [ py_2, text_sm ] ]
                        [ span
                            [ css [ font_bold, mr_2 ]
                            ]
                            [ [ String.fromInt (i + 1)
                              , "."
                              , " "
                              , story.title
                              ]
                                |> String.join ""
                                |> text
                            ]
                        , span
                            [ css [ text_gray_500, font_light ]
                            ]
                            [ [ "(", story.source, ")" ]
                                |> String.join ""
                                |> text
                            ]
                        ]
                )
        )


type alias Story =
    { title : String
    , source : String
    }


stories =
    [ Story "Perl turns 30 today" "github.com/perl"
    , Story "Internet addiction and the habit of book reading" "benwajdi.com"
    , Story "Charles Babbage – Passages from the Life of a Philosopher" "standardebooks.org"
    ]
