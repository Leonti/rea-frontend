module Data.DatePrice exposing (DatePrice, decoder, graphQlSpec)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import GraphQL.Request.Builder exposing (..)


type alias DatePrice =
    { price : Int
    , timestamp : Int
    }


graphQlSpec : ValueSpec NonNull ObjectType DatePrice vars
graphQlSpec =
    object DatePrice
        |> with (field "price" [] int)
        |> with (field "timestamp" [] int)


decoder : Decoder DatePrice
decoder =
    decode DatePrice
        |> required "price" Decode.int
        |> required "timestamp" Decode.int
