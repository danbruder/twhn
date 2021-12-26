module Domain.Comment exposing (Comment)


type alias Comment =
    { id : Int
    , text : String
    , by : String
    , humanTime : String
    , parent : Int
    , kids : List Int
    }
