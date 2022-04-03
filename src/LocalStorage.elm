port module LocalStorage exposing (get, got, set)

import Json.Encode as JE


port set : { key : String, value : JE.Value } -> Cmd msg


port got : (( String, JE.Value ) -> msg) -> Sub msg


port get : String -> Cmd msg
