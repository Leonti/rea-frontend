module Data.SoldProperty exposing (SoldProperty, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias SoldProperty =
    { location : String
    , link : String
    , bathrooms : Int
    , bedrooms : Int
    , cars : Int
    , soldAt : String
    , price : Int
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
