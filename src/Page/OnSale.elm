module Page.OnSale exposing (Model, Msg, initialModel, update, view, subscriptions)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Events exposing (on)
import Html.Attributes exposing (value, selected)
import Date exposing (Date, fromTime)
import Time.Date as LocalDate
import Http exposing (encodeUri)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty, propertyFirstDates, propertyDates, newForDate)
import Data.DateCount as DateCount exposing (DateCount)
import Data.DatePrice as DatePrice exposing (DatePrice)
import Data.Distances as Distances exposing (Distances)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format as Format exposing (format)
import Data.Stats exposing (medianPrices, medianPriceForDate)
import Time exposing (Time)
import Route as Route exposing (Route(..))
import Json.Decode as Json


-- MODEL --


type alias Model =
    { selectedDate : Maybe LocalDate.Date
    , selectedId : Maybe String
    }


initialModel : Maybe LocalDate.Date -> Maybe String -> Model
initialModel selectedDate selectedId =
    { selectedDate = selectedDate
    , selectedId = selectedId
    }


hasLocation : OnSaleProperty -> Bool
hasLocation onSaleProperty =
    case onSaleProperty.geo of
        Just geo ->
            True

        Nothing ->
            False



-- UPDATE --


type Msg
    = CurrentTime Time
    | SetDate String


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        CurrentTime time ->
            ( model, Cmd.none )

        SetDate dateAsString ->
            case LocalDate.fromISO8601 dateAsString of
                Ok date ->
                    ( { model | selectedDate = Just date }, Route.modifyUrl (Route.OnSaleForDate date) )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW --


isInLast : LocalDate.Date -> OnSaleProperty -> Bool
isInLast date onSaleProperty =
    List.member date <| propertyDates onSaleProperty


view : Session -> Time -> Maybe (List DateCount) -> Maybe (List OnSaleProperty) -> Model -> Html Msg
view session currentTime maybeDateCounts maybeOnSaleProperties model =
    let
        propertiesWithLocation =
            List.filter hasLocation (Maybe.withDefault [] maybeOnSaleProperties)

        dateSelectorView =
            case maybeDateCounts of
                Just dateCounts ->
                    dateSelector model.selectedDate dateCounts

                Nothing ->
                    div [] [ text "Loading dates" ]

        detailsView =
            case Maybe.andThen (findProperty propertiesWithLocation) model.selectedId of
                Just selectedProperty ->
                    propertyDetailsView selectedProperty

                Nothing ->
                    summaryView propertiesWithLocation model.selectedDate
    in
        div [ class "home-page" ]
            [ div [ class "container-fluid" ]
                [ div [ class "row" ]
                    [ div [ class "col" ]
                        [ text <| toString (List.length propertiesWithLocation) ++ " properties" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col" ]
                        [ dateSelectorView
                        , viewOnSaleProperties model.selectedDate propertiesWithLocation
                        ]
                    , div [ class "col" ]
                        [ detailsView
                        ]
                    ]
                ]
            ]


findProperty : List OnSaleProperty -> String -> Maybe OnSaleProperty
findProperty properties propertyId =
    List.head <| List.filter (\p -> p.link == "/" ++ propertyId) properties


propertyDetailsView : OnSaleProperty -> Html Msg
propertyDetailsView property =
    div [ class "row" ]
        [ div [ class "col" ]
            [ div []
                [ span [] [ text property.location ] ]
            , priceChartView property.datesPrices
            , propertyMap property.location
            ]
        ]


propertyMap : String -> Html Msg
propertyMap location =
    let
        encoded =
            encodeUri <| location ++ ", Australia"

        iframeSrc =
            "https://www.google.com/maps/embed/v1/place?key=AIzaSyCZ4yEZDvm3tUTl1Kz5xfmlNi93iio3-gY&q=" ++ encoded
    in
        div [ class "property-map" ]
            [ iframe [ src iframeSrc ] []
            ]


priceChartView : List DatePrice -> Html Msg
priceChartView datePrices =
    let
        chartUrl =
            "https://chart.googleapis.com/chart?cht=lc&chs=800x150&chd=t:" ++ (chartPrices datePrices) ++ "&chds=a&chxt=y&chxs=1,0000ff,10,1,lt"
    in
        div []
            [ img [ src chartUrl ] []
            ]


chartPrices : List DatePrice -> String
chartPrices datePrices =
    let
        stringDates =
            List.map (toString << .price) datePrices
    in
        String.join "," stringDates


summaryView : List OnSaleProperty -> Maybe LocalDate.Date -> Html Msg
summaryView onSaleProperties maybeSelectedDate =
    div [ class "row" ]
        [ div [ class "col" ]
            [ span [] [ text <| "Total properties count: " ++ toString (List.length onSaleProperties) ]
            ]
        ]


dateSelector : Maybe LocalDate.Date -> List DateCount -> Html Msg
dateSelector maybeSelectedDate dateCounts =
    select [ on "change" (Json.map SetDate targetValueString) ]
        (List.map (dateOption maybeSelectedDate) dateCounts)


targetValueString : Json.Decoder String
targetValueString =
    Json.at [ "target", "value" ] Json.string


dateOption : Maybe LocalDate.Date -> DateCount -> Html msg
dateOption maybeSelectedDate dateCount =
    let
        isSelected =
            Maybe.withDefault False <| Maybe.map (\selectedDate -> selectedDate == dateCount.date) maybeSelectedDate
    in
        option [ value (LocalDate.toISO8601 dateCount.date), selected isSelected ] [ text <| (LocalDate.toISO8601 dateCount.date) ++ " " ++ toString dateCount.count ]


propertyLink : Maybe LocalDate.Date -> String -> Html msg
propertyLink maybeSelectedDate propertyId =
    div []
        [ a [ Route.href (OnSaleForDateExpanded maybeSelectedDate propertyId) ] [ text "Details" ]
        ]


viewOnSaleProperties : Maybe LocalDate.Date -> List OnSaleProperty -> Html msg
viewOnSaleProperties maybeSelectedDate onSaleProperties =
    div [ class "property-list-wrapper" ]
        [ div [ class "list-group" ] (List.map (viewOnSaleProperty maybeSelectedDate) onSaleProperties)
        ]


viewOnSaleProperty : Maybe LocalDate.Date -> OnSaleProperty -> Html msg
viewOnSaleProperty maybeSelectedDate onSaleProperty =
    div
        [ class "list-group-item list-group-item-action flex-column align-items-start"
        ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ viewPropertyHeader onSaleProperty True
                , viewSoldDetails onSaleProperty
                , viewPropertyDetails onSaleProperty
                , viewPropertyDistances onSaleProperty
                , propertyLink maybeSelectedDate onSaleProperty.link
                ]
            ]
        ]


viewNotListedBadge : Bool -> Html msg
viewNotListedBadge isListed =
    case isListed of
        True ->
            span [] []

        False ->
            span []
                [ span [] [ text " " ]
                , span [ class "badge badge-secondary" ] [ text "NOT LISTED" ]
                ]


viewPropertyHeader : OnSaleProperty -> Bool -> Html msg
viewPropertyHeader onSaleProperty isListed =
    let
        badge =
            if onSaleProperty.isSold then
                span []
                    [ span [] [ text " " ]
                    , span [ class "badge badge-secondary" ] [ text "SOLD" ]
                    ]
            else
                viewNotListedBadge isListed

        days =
            List.length onSaleProperty.datesPrices
    in
        div [ class "row" ]
            [ div [ class "col-7" ]
                [ a [ href <| "https://realestate.com.au" ++ onSaleProperty.link ]
                    [ h5 [ class "mb-1" ]
                        [ text (onSaleProperty.location)
                        , badge
                        ]
                    ]
                ]
            , div [ class "col" ]
                [ span [] [ text <| (toString days) ++ " days on sale" ] ]
            , div [ class "col" ]
                [ span [ class "text-muted" ] [ text <| formattedTimestamp onSaleProperty.extractedAt ] ]
            , div [ class "col" ]
                [ div [ class "float-right" ] [ viewLastPrice onSaleProperty ] ]
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
        span [] [ text <| "$" ++ (toString lastPrice) ]


viewSoldDetails : OnSaleProperty -> Html msg
viewSoldDetails onSaleProperty =
    if onSaleProperty.isSold then
        let
            salePrice =
                toString <| Maybe.withDefault 0 onSaleProperty.salePrice

            saleDate =
                Maybe.withDefault "" (Maybe.map formattedTimestamp onSaleProperty.soldAt)
        in
            div [ class "row" ]
                [ div [ class "col-7" ] []
                , div [ class "col" ]
                    [ text "Sold on" ]
                , div [ class "col" ]
                    [ span [ class "text-muted" ] [ text saleDate ] ]
                , div [ class "col" ]
                    [ div [ class "float-right" ] [ text <| "$" ++ salePrice ] ]
                ]
    else
        div [] []


viewPropertyDistances : OnSaleProperty -> Html msg
viewPropertyDistances onSaleProperty =
    case onSaleProperty.distances of
        Just distances ->
            viewDistances distances

        Nothing ->
            div [] []


viewDistances : Distances -> Html msg
viewDistances distances =
    div [ class "row" ]
        [ div [ class "col" ] [ text <| "Aldi: " ++ (toString distances.toAldi) ]
        , div [ class "col" ] [ text <| "Woolworth: " ++ (toString distances.toWoolworth) ]
        , div [ class "col" ] [ text <| "Coles: " ++ (toString distances.toColes) ]
        , div [ class "col" ] [ text <| "Train: " ++ (toString distances.toTrain) ]
        , div [ class "col" ] [ text <| "Tram: " ++ (toString distances.toTram) ]
        , div [ class "col" ] [ text <| "Bus: " ++ (toString distances.toBus) ]
        ]


viewPropertyDetails : OnSaleProperty -> Html msg
viewPropertyDetails onSaleProperty =
    div []
        [ span [ class "property-details" ]
            [ svg [ Svg.Attributes.class "property-detail-icon" ] [ use [ xlinkHref "assets/sprite.svg#si-glyph-bed" ] [] ]
            , span [ class "property-details-count" ] [ text <| " " ++ toString onSaleProperty.bedrooms ]
            ]
        , span [ class "property-details" ]
            [ svg [ Svg.Attributes.class "property-detail-icon" ] [ use [ xlinkHref "assets/sprite.svg#si-glyph-shower" ] [] ]
            , span [ class "property-details-count" ] [ text <| " " ++ toString onSaleProperty.bathrooms ]
            ]
        , span [ class "property-details" ]
            [ svg [ Svg.Attributes.class "property-detail-icon" ] [ use [ xlinkHref "assets/sprite.svg#si-glyph-car" ] [] ]
            , span [ class "property-details-count" ] [ text <| " " ++ toString onSaleProperty.cars ]
            ]
        ]



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second * 5) CurrentTime
        ]
