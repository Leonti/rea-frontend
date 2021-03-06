module Data.SoldProperty exposing (SoldProperty, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Data.Geo as Geo exposing (Geo)
import Data.Distances as Distances exposing (Distances)


type alias SoldProperty =
    { location : String
    , link : String
    , bathrooms : Int
    , bedrooms : Int
    , cars : Int
    , soldAt : String
    , price : Int
    , geo : Maybe Geo
    , distances : Maybe Distances
    }


decoder : Decoder SoldProperty
decoder =
    decode SoldProperty
        |> required "location" Decode.string
        |> required "link" Decode.string
        |> required "bathrooms" Decode.int
        |> required "bedrooms" Decode.int
        |> required "cars" Decode.int
        |> required "soldAt" Decode.string
        |> required "price" Decode.int
        |> optional "geo" (Decode.nullable Geo.decoder) Nothing
        |> optional "distances" (Decode.nullable Distances.decoder) Nothing
