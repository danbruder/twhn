module Domain.Comment exposing (Comment, hasKids)


type alias Comment =
    { id : Int
    , text : String
    , by : String
    , humanTime : String
    , parent : Int
    , kids : List Int
    }


hasKids : Comment -> Bool
hasKids comment =
    List.isEmpty comment.kids |> not
