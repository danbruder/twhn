module Domain.Story exposing (..)

import Url exposing (Url)


type alias Story =
    { id : Int
    , title : String
    , safeText : String
    , url : Maybe Url
    , by : String
    , score : Int
    , humanTime : String
    , kids : List Int
    }
