module Api exposing (makeRequest)

import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet


makeRequest :
    (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Graphql.SelectionSet.SelectionSet decodesTo Graphql.Operation.RootQuery
    -> Cmd msg
makeRequest toMsg query =
    query
        --|> Graphql.Http.queryRequest "https://dawn-forest-4900.fly.dev"
        |> Graphql.Http.queryRequest "http://localhost:8000"
        |> Graphql.Http.send toMsg
