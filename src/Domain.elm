module Domain exposing (..)

import Url exposing (Url)


type alias Story =
    { id : Int
    , title : String
    , url : Maybe Url
    , comments : List Comment
    }


type alias Comment =
    { id : Int
    , text : String
    }
