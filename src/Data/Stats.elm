module Data.Stats exposing (medianPrices, medianPriceForDate)

import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty, propertyFirstDates, priceForDate)
import Time.Date as LocalDate exposing (Date, date)
import Statistics exposing (median)


medianPrices : List OnSaleProperty -> List Float
medianPrices onSalePropertyList =
    let
        dates =
            propertyFirstDates onSalePropertyList
    in
        List.map (medianPriceForDate onSalePropertyList) dates


medianPriceForDate : List OnSaleProperty -> Date -> Float
medianPriceForDate onSalePropertyList date =
    let
        maybePrices =
            List.map (priceForDate date) onSalePropertyList

        prices =
            List.map toFloat <| List.map (Maybe.withDefault 0) maybePrices
    in
        median prices
