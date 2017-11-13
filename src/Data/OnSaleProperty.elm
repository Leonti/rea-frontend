module Data.OnSaleProperty exposing (OnSaleProperty, decoder, propertyDates)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Data.DatePrice as DatePrice exposing (DatePrice)
import Data.Geo as Geo exposing (Geo)
import Data.Distances as Distances exposing (Distances)
import Date exposing (Date, year, month, day)
import Date.Extra.Create exposing (dateFromFields)
import List.Extra exposing (uniqueBy)


type alias OnSaleProperty =
    { location : String
    , link : String
    , bathrooms : Int
    , bedrooms : Int
    , cars : Int
    , extractedAt : Int
    , isSold : Bool
    , soldAt : Maybe Int
    , salePrice : Maybe Int
    , datesPrices : List DatePrice
    , geo : Maybe Geo
    , distances : Maybe Distances
    }


decoder : Decoder OnSaleProperty
decoder =
    decode OnSaleProperty
        |> required "location" Decode.string
        |> required "link" Decode.string
        |> required "bathrooms" Decode.int
        |> required "bedrooms" Decode.int
        |> required "cars" Decode.int
        |> required "extractedAt" Decode.int
        |> required "isSold" Decode.bool
        |> optional "soldAt" (Decode.nullable Decode.int) Nothing
        |> optional "salePrice" (Decode.nullable Decode.int) Nothing
        |> required "datesPrices" (Decode.list DatePrice.decoder)
        |> optional "geo" (Decode.nullable Geo.decoder) Nothing
        |> optional "distances" (Decode.nullable Distances.decoder) Nothing


propertyDates : List OnSaleProperty -> List Date
propertyDates onSaleProperties =
    let
        firstDates =
            List.filterMap firstDate onSaleProperties
    in
        uniqueBy Date.toTime firstDates


firstDate : OnSaleProperty -> Maybe Date
firstDate onSaleProperty =
    let
        maybeFirstTimestamp =
            Maybe.map .timestamp <| List.head onSaleProperty.datesPrices
    in
        Maybe.map (\t -> zeroedDate <| Date.fromTime (toFloat t * 1000)) maybeFirstTimestamp


zeroedDate : Date -> Date
zeroedDate d =
    dateFromFields (year d) (month d) (day d) 0 0 0 0
