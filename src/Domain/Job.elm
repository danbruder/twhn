module Domain.Job exposing (..)

import Url exposing (Url)


type alias Job =
    { id : Int
    , score : Int
    , safeText : String
    , url : Maybe Url
    , humanTime : String
    , title : String
    }
