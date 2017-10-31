module Data.Geo exposing (Geo, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Geo =
    { formattedAddress : String
    , latitude : Float
    , longitude : Float
    }


decoder : Decoder Geo
decoder =
    decode Geo
        |> required "formattedAddress" Decode.string
        |> required "latitude" Decode.float
        |> required "longitude" Decode.float
