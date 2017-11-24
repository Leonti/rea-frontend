module Data.OnSaleProperty exposing (OnSaleProperty, decoder, propertyFirstDates, propertyDates, newForDate)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Data.DatePrice as DatePrice exposing (DatePrice)
import Data.Geo as Geo exposing (Geo)
import Data.Distances as Distances exposing (Distances)
import Date exposing (year, month, day)
import Time.Date as LocalDate exposing (Date, date)
import Date.Extra.Core exposing (monthToInt)
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


propertyFirstDates : List OnSaleProperty -> List Date
propertyFirstDates onSaleProperties =
    let
        firstDates =
            List.filterMap firstDate onSaleProperties
    in
        uniqueBy LocalDate.toTuple firstDates


propertyDates : OnSaleProperty -> List Date
propertyDates onSaleProperty =
    let
        timestamps =
            List.map .timestamp onSaleProperty.datesPrices
    in
        List.map timestampToDate timestamps


newForDate : List OnSaleProperty -> Date -> List OnSaleProperty
newForDate onSaleProperties date =
    List.filterMap (isForDate date) onSaleProperties


isForDate : Date -> OnSaleProperty -> Maybe OnSaleProperty
isForDate date onSaleProperty =
    case firstDate onSaleProperty of
        Just propertyDate ->
            if propertyDate == date then
                Just onSaleProperty
            else
                Nothing

        Nothing ->
            Nothing


firstDate : OnSaleProperty -> Maybe Date
firstDate onSaleProperty =
    let
        maybeFirstTimestamp =
            Maybe.map .timestamp <| List.head onSaleProperty.datesPrices
    in
        Maybe.map timestampToDate maybeFirstTimestamp


timestampToDate : Int -> Date
timestampToDate t =
    zeroedDate <| Date.fromTime (toFloat t * 1000)


zeroedDate : Date.Date -> Date
zeroedDate d =
    date (year d) (monthToInt <| month d) (day d)
