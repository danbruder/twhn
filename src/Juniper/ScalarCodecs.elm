-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Juniper.ScalarCodecs exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Juniper.Scalar exposing (defaultCodecs)


type alias Id =
    Juniper.Scalar.Id


codecs : Juniper.Scalar.Codecs Id
codecs =
    Juniper.Scalar.defineCodecs
        { codecId = defaultCodecs.codecId
        }
