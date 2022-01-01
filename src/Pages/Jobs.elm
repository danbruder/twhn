module Pages.Jobs exposing (Model, Msg, page)

import Components.List as CL
import Dict exposing (Dict)
import Gen.Params.Jobs exposing (Params)
import Juniper.Query as Query
import Page
import Request
import Shared


key =
    "Jobs"


query =
    Query.jobItems


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init =
            CL.init
                { cachedList = Dict.get key shared.listIndex |> Maybe.withDefault []
                , allItems = shared.items
                , query = query
                }
        , update = CL.update key query
        , view = CL.view key req.route
        , subscriptions = CL.subscriptions
        }


type alias Msg =
    CL.Msg


type alias Model =
    CL.Model
