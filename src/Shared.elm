module Shared exposing
    ( Flags
    , Model
    , Msg
    , gotItems
    , gotListIndex
    , init
    , subscriptions
    , update
    )

import Api
import Dict exposing (Dict)
import Domain.Item as Item exposing (Item)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Json.Decode as Json
import Juniper.Object.Stats as Stats
import Juniper.Query as Query
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { items : Dict Int Item
    , listIndex : Dict String (List Int)
    , stats : Maybe Stats
    }


type Msg
    = GotItem Item
    | GotItems (Dict Int Item)
    | GotListIndex String (List Int)
    | GotStats (Result (Graphql.Http.Error Stats) Stats)


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { items = Dict.empty, listIndex = Dict.empty, stats = Nothing }, getStats )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        GotItem item ->
            ( { model | items = Dict.insert (Item.id item) item model.items }, Cmd.none )

        GotItems items ->
            ( { model | items = Dict.union items model.items }
            , Cmd.none
            )

        GotListIndex key ids ->
            ( { model | listIndex = Dict.insert key ids model.listIndex }
            , Cmd.none
            )

        GotStats response ->
            case response of
                Ok stats ->
                    ( { model | stats = Just stats }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


gotItems : List Item -> Msg
gotItems items =
    GotItems
        (items
            |> List.map (\item -> ( Item.id item, item ))
            |> Dict.fromList
        )


gotListIndex : String -> List Int -> Msg
gotListIndex key topItems =
    GotListIndex key topItems


getStats : Cmd Msg
getStats =
    Query.stats
        (SelectionSet.succeed Stats
            |> SelectionSet.with Stats.itemCount
        )
        |> Api.makeRequest GotStats


type alias Stats =
    { itemCount : Int
    }
