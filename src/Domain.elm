module Domain exposing (..)

import Url exposing (Url)


type alias Story =
    { id : Int
    , title : String
    , url : Maybe Url
    }


type alias Comment =
    { id : Int
    , text : String
    }


type alias StoryWithComments =
    { id : Int
    , title : String
    , url : Maybe Url
    , comments : List Comment
    }
