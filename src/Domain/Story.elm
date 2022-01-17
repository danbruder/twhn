module Domain.Story exposing (..)

import Time exposing (Posix)
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
    , ranks : List Rank
    }


type alias Rank =
    { value : Int
    , ts : Posix
    }
