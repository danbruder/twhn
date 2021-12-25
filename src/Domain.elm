module Domain exposing (..)

import Url exposing (Url)


type alias Story =
    { id : Int
    , title : String
    , url : Maybe Url
    , comments : List Comment
    , by : String
    , score : Int
    , humanTime : String
    , kids : List Int
    }


type Comment
    = Comment
        { id : Int
        , text : String
        , by : String
        , humanTime : String
        , comments : List Comment
        , parent : Int
        , kids : List Int
        }


newComment : Int -> String -> String -> String -> List Comment -> Int -> List Int -> Comment
newComment id text by humanTime comments parent kids =
    Comment
        { id = id
        , text = text
        , by = by
        , humanTime = humanTime
        , comments = comments
        , parent = parent
        , kids = kids
        }
