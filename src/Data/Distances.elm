module Data.Distances exposing (Distances, decoder, graphQlSpec)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import GraphQL.Request.Builder exposing (..)


type alias Distances =
    { toTrain : Int
    , toWoolworth : Int
    , toBus : Int
    , toColes : Int
    , toAldi : Int
    , toTram : Int
    }


graphQlSpec : ValueSpec NonNull ObjectType Distances vars
graphQlSpec =
    object Distances
        |> with (field "toTrain" [] int)
        |> with (field "toWoolworth" [] int)
        |> with (field "toBus" [] int)
        |> with (field "toColes" [] int)
        |> with (field "toAldi" [] int)
        |> with (field "toTram" [] int)


decoder : Decoder Distances
decoder =
    decode Distances
        |> required "toTrain" Decode.int
        |> required "toWoolworth" Decode.int
        |> required "toBus" Decode.int
        |> required "toColes" Decode.int
        |> required "toAldi" Decode.int
        |> required "toTram" Decode.int
