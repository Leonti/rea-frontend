module Page.OnSale exposing (Model, Msg, initialModel, update, view, subscriptions)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Date exposing (Date, fromTime)
import Time.Date as LocalDate
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty, propertyFirstDates, propertyDates, newForDate)
import Data.Distances as Distances exposing (Distances)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format as Format exposing (format)
import Time exposing (Time)
import Route as Route exposing (Route(..))


-- MODEL --


type alias Model =
    { selectedDate : Maybe LocalDate.Date
    }


initialModel : Maybe LocalDate.Date -> Model
initialModel selectedDate =
    { selectedDate = selectedDate
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


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        CurrentTime time ->
            ( model, Cmd.none )



-- VIEW --


isInLast : LocalDate.Date -> OnSaleProperty -> Bool
isInLast date onSaleProperty =
    List.member date <| propertyDates onSaleProperty


view : Session -> Time -> Maybe (List OnSaleProperty) -> Model -> Html Msg
view session currentTime maybeOnSaleProperties model =
    let
        propertiesWithLocation =
            List.filter hasLocation (Maybe.withDefault [] maybeOnSaleProperties)

        onSaleProperties =
            case model.selectedDate of
                Just date ->
                    newForDate propertiesWithLocation date

                Nothing ->
                    propertiesWithLocation

        sortedDates =
            List.sortBy LocalDate.toTuple (propertyFirstDates propertiesWithLocation)

        maybeLast =
            List.head <| List.reverse sortedDates

        onSalePropertiesView =
            case maybeLast of
                Just date ->
                    viewOnSaleProperties date onSaleProperties

                Nothing ->
                    div [] []
    in
        div [ class "home-page" ]
            [ div [ class "container" ]
                [ div [ class "row" ]
                    [ text <| toString (List.length onSaleProperties) ]
                , div [ class "row" ]
                    (List.map dateLink sortedDates)
                , div [ class "row" ]
                    [ div [ class "col" ]
                        [ onSalePropertiesView
                        ]
                    ]
                ]
            ]


dateLink : LocalDate.Date -> Html msg
dateLink date =
    span []
        [ a [ Route.href (OnSaleForDate date) ] [ text <| LocalDate.toISO8601 date ]
        , span [] [ text "_" ]
        ]


viewOnSaleProperties : LocalDate.Date -> List OnSaleProperty -> Html msg
viewOnSaleProperties lastDate onSaleProperties =
    div [ class "list-group" ] (List.map (viewOnSaleProperty lastDate) onSaleProperties)


viewOnSaleProperty : LocalDate.Date -> OnSaleProperty -> Html msg
viewOnSaleProperty lastDate onSaleProperty =
    div
        [ class "list-group-item list-group-item-action flex-column align-items-start"
        ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ viewPropertyHeader onSaleProperty
            , span [ class "text-muted" ] [ text <| formattedTimestamp onSaleProperty.extractedAt ]
            , viewPropertyDetails onSaleProperty
            , viewLastPrice onSaleProperty
            , viewPropertyStats lastDate onSaleProperty
            , viewPropertyDistances onSaleProperty
            ]
        ]


viewPropertyHeader : OnSaleProperty -> Html msg
viewPropertyHeader onSaleProperty =
    let
        badge =
            if onSaleProperty.isSold then
                span []
                    [ span [] [ text " " ]
                    , span [ class "badge badge-secondary" ] [ text "SOLD" ]
                    ]
            else
                span [] []
    in
        a [ href <| "https://realestate.com.au" ++ onSaleProperty.link ]
            [ h5 [ class "mb-1" ]
                [ text (onSaleProperty.location)
                , badge
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


viewPropertyStats : LocalDate.Date -> OnSaleProperty -> Html msg
viewPropertyStats lastDate onSaleProperty =
    let
        days =
            List.length onSaleProperty.datesPrices

        salePrice =
            toString <| Maybe.withDefault 0 onSaleProperty.salePrice

        saleDate =
            Maybe.withDefault "" (Maybe.map formattedTimestamp onSaleProperty.soldAt)

        sold =
            if onSaleProperty.isSold then
                " SOLD for $" ++ salePrice ++ " on " ++ saleDate
            else
                ""

        notListed =
            if isInLast lastDate onSaleProperty then
                ""
            else
                " NOT LISTED"
    in
        div [] [ text <| "On sale for " ++ (toString days) ++ " days" ++ sold ++ notListed ]


viewPropertyDistances : OnSaleProperty -> Html msg
viewPropertyDistances onSaleProperty =
    case onSaleProperty.distances of
        Just distances ->
            viewDistances distances

        Nothing ->
            div [] []


viewDistances : Distances -> Html msg
viewDistances distances =
    div []
        [ div [] [ text <| "Aldi:" ++ (toString distances.toAldi) ]
        , div [] [ text <| "Woolworth:" ++ (toString distances.toWoolworth) ]
        , div [] [ text <| "Coles:" ++ (toString distances.toColes) ]
        , div [] [ text <| "Train:" ++ (toString distances.toTrain) ]
        , div [] [ text <| "Tram:" ++ (toString distances.toTram) ]
        , div [] [ text <| "Bus:" ++ (toString distances.toBus) ]
        ]


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



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second * 5) CurrentTime
        ]
