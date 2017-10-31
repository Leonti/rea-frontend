module Data.Distances exposing (Distances, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Distances =
    { toTrain : Int
    , toWoolworth : Int
    , toBus : Int
    , toColes : Int
    , toAldi : Int
    , toTram : Int
    }


decoder : Decoder Distances
decoder =
    decode Distances
        |> required "toTrain" Decode.int
        |> required "toWoolworth" Decode.int
        |> required "toBus" Decode.int
        |> required "toColes" Decode.int
        |> required "toAldi" Decode.int
        |> required "toTram" Decode.int
