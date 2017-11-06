module Data.Geo exposing (Geo, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Geo =
    { latitude : Float
    , longitude : Float
    }


decoder : Decoder Geo
decoder =
    decode Geo
        |> required "latitude" Decode.float
        |> required "longitude" Decode.float
