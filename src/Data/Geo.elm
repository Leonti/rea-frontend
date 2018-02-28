module Data.Geo exposing (Geo, decoder, graphQlSpec)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import GraphQL.Request.Builder exposing (..)


type alias Geo =
    { latitude : Float
    , longitude : Float
    }


graphQlSpec : ValueSpec NonNull ObjectType Geo vars
graphQlSpec =
    object Geo
        |> with (field "latitude" [] float)
        |> with (field "longitude" [] float)


decoder : Decoder Geo
decoder =
    decode Geo
        |> required "latitude" Decode.float
        |> required "longitude" Decode.float
