module Domain exposing (Story)

import Url exposing (Url)


type alias Story =
    { id : Int
    , title : String
    , url : Maybe Url
    }
