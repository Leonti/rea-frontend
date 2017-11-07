module Page.OnSale exposing (Model, init, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Date exposing (Date, fromTime)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.OnSaleProperty
import Task exposing (Task)
import Views.Page as Page
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format as Format exposing (format)


-- MODEL --


type alias Model =
    { onSaleProperties : List OnSaleProperty
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadOnSaleProperties =
            Request.OnSaleProperty.all session.maybeAuthToken
                |> Http.toTask

        toModel properties =
            Model <| List.filter hasLocation properties

        handleLoadError _ =
            pageLoadError Page.OnSale "Could not load on sale properties"
    in
        Task.map toModel loadOnSaleProperties
            |> Task.mapError handleLoadError


hasLocation : OnSaleProperty -> Bool
hasLocation onSaleProperty =
    case onSaleProperty.geo of
        Just geo ->
            True

        Nothing ->
            False



-- VIEW --


view : Session -> Model -> Html msg
view session model =
    div [ class "home-page" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ viewOnSaleProperties model.onSaleProperties
                    ]
                ]
            ]
        ]


viewOnSaleProperties : List OnSaleProperty -> Html msg
viewOnSaleProperties onSaleProperties =
    div [ class "list-group" ] (List.map viewOnSaleProperty onSaleProperties)


viewOnSaleProperty : OnSaleProperty -> Html msg
viewOnSaleProperty onSaleProperty =
    div
        [ class "list-group-item list-group-item-action flex-column align-items-start"
        ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ text (onSaleProperty.location) ]
            , span [ class "text-muted" ] [ text <| formattedTimestamp onSaleProperty.extractedAt ]
            , a [href <| "https://realestate.com.au" ++ onSaleProperty.link] [text "Link"]
            , viewPropertyDetails onSaleProperty
            , viewLastPrice onSaleProperty
            , viewPropertyStats onSaleProperty
            ]
        ]


formattedTimestamp : Int -> String
formattedTimestamp t =
    (format config config.format.date) <| timestampToDate t


timestampToDate : Int -> Date
timestampToDate t =
    Date.fromTime (toFloat <| t * 1000)


viewLastPrice : OnSaleProperty -> Html msg
viewLastPrice onSaleProperty =
    let
        lastPrice =
            Maybe.withDefault 0 <| Maybe.map .price (List.head <| List.sortBy .timestamp onSaleProperty.datesPrices)
    in
        div [] [ text <| "$" ++ (toString lastPrice) ]


viewPropertyStats : OnSaleProperty -> Html msg
viewPropertyStats onSaleProperty =
    let
        days =
            List.length onSaleProperty.datesPrices
    in
        div [] [ text <| "On sale for " ++ (toString days) ++ " days" ]


viewPropertyDetails : OnSaleProperty -> Html msg
viewPropertyDetails onSaleProperty =
    div []
        [ svg [ Svg.Attributes.class "property-detail-icon" ] [ use [ xlinkHref "assets/sprite.svg#si-glyph-bed" ] [] ]
        , span [] [ text <| toString onSaleProperty.bedrooms ]
        , svg [ Svg.Attributes.class "property-detail-icon" ] [ use [ xlinkHref "assets/sprite.svg#si-glyph-shower" ] [] ]
        , span [] [ text <| toString onSaleProperty.bathrooms ]
        , svg [ Svg.Attributes.class "property-detail-icon" ] [ use [ xlinkHref "assets/sprite.svg#si-glyph-car" ] [] ]
        , span [] [ text <| toString onSaleProperty.cars ]
        ]
