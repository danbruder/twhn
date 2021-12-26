module Shared exposing
    ( Flags
    , Model
    , Msg
    , gotItems
    , gotTopItems
    , init
    , subscriptions
    , update
    )

import Dict exposing (Dict)
import Domain.Item as Item exposing (Item)
import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { items : Dict Int Item
    , topItems : List Int
    }


type Msg
    = GotItem Item
    | GotItems (Dict Int Item)
    | GotTopItems (List Int)


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { items = Dict.empty, topItems = [] }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        GotItem item ->
            ( { model | items = Dict.insert (Item.id item) item model.items }, Cmd.none )

        GotItems items ->
            ( { model | items = Dict.union items model.items }
            , Cmd.none
            )

        GotTopItems topItems ->
            ( { model | topItems = topItems }
            , Cmd.none
            )


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


gotTopItems : List Int -> Msg
gotTopItems topItems =
    GotTopItems topItems
