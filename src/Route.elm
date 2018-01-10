module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import String
import Navigation exposing (Location)
import Data.AuthToken exposing (AuthToken, extractAccessToken)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string, custom)
import Time.Date as LocalDate exposing (Date, fromISO8601, toISO8601)


-- ROUTING --


type Route
    = Home (Maybe AuthToken)
    | Login
    | Sold
    | OnSale
    | OnSaleForDate Date
    | OnSaleForDateExpanded (Maybe Date) String


accessTokenParser : Parser (AuthToken -> a) a
accessTokenParser =
    custom "ACCESS_TOKEN" extractToken


extractToken : String -> Result String AuthToken
extractToken hash =
    case extractAccessToken hash of
        Just ( token, expiresIn ) ->
            Ok
                { token = token
                , expiresIn = expiresIn
                }

        Nothing ->
            Err "no access token present"


dateParser : Parser (Date -> a) a
dateParser =
    custom "DATE_PARSER" fromISO8601


maybeDateParser : Parser (Maybe Date -> a) a
maybeDateParser =
    Url.map Just dateParser


onSaleExpandedWithoutDate : String -> Route
onSaleExpandedWithoutDate =
    OnSaleForDateExpanded Nothing


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map tokenToRoute accessTokenParser
        , Url.map (Home Nothing) (s "")
        , Url.map Login (s "login")
        , Url.map Sold (s "sold")
        , Url.map OnSaleForDateExpanded (s "on-sale" </> maybeDateParser </> string)
        , Url.map OnSaleForDate (s "on-sale" </> dateParser)
        , Url.map onSaleExpandedWithoutDate (s "on-sale" </> string)
        , Url.map OnSale (s "on-sale")
        ]


tokenToRoute : AuthToken -> Route
tokenToRoute token =
    Home <| Just token



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home _ ->
                    []

                Login ->
                    [ "login" ]

                Sold ->
                    [ "sold" ]

                OnSale ->
                    [ "on-sale" ]

                OnSaleForDateExpanded maybeSelectedDate propertyId ->
                    case maybeSelectedDate of
                        Just date ->
                            [ "on-sale", toISO8601 date, String.dropLeft 1 propertyId ]

                        Nothing ->
                            [ "on-sale", String.dropLeft 1 propertyId ]

                OnSaleForDate date ->
                    [ "on-sale", toISO8601 date ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just (Home Nothing)
    else
        parseHash route location
