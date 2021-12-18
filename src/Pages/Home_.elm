module Pages.Home_ exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body = [ body ]
    }


body =
    div
        [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"
        ]
        [ div [ class "max-w-2xl mx-auto  " ]
            [ div [ class "p-4 border" ]
                [ h1 [ class "font-bold text-2xl" ] [ text "Home" ]
                ]
            , ul [ class "p-4 border" ] <|
                (stories
                    |> List.indexedMap
                        (\i story ->
                            li [ class "py-2" ]
                                [ span [ class "font-bold mr-2" ]
                                    [ [ String.fromInt (i + 1)
                                      , "."
                                      , " "
                                      , story.title
                                      ]
                                        |> String.join ""
                                        |> text
                                    ]
                                , span [ class "text-gray-500 font-light" ]
                                    [ [ "(", story.source, ")" ]
                                        |> String.join ""
                                        |> text
                                    ]
                                ]
                        )
                )
            ]
        ]


type alias Story =
    { title : String
    , source : String
    }


stories =
    [ Story "Perl turns 30 today" "github.com/perl"
    , Story "Internet addiction and the habit of book reading" "benwajdi.com"
    , Story "Charles Babbage – Passages from the Life of a Philosopher" "standardebooks.org"
    ]
