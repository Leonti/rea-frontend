module Data.DatePrice exposing (DatePrice, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias DatePrice =
    { price : Int
    , timestamp : Int
    }


decoder : Decoder DatePrice
decoder =
    decode DatePrice
        |> required "price" Decode.int
        |> required "timestamp" Decode.int
