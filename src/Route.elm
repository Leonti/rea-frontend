module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import String
import Navigation exposing (Location)
import Data.AuthToken exposing (AuthToken, extractAccessToken)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string, custom)


-- ROUTING --


type Route
    = Home (Maybe AuthToken)
    | Login
    | Sold
    | OnSale


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


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map tokenToRoute accessTokenParser
        , Url.map (Home Nothing) (s "")
        , Url.map Login (s "login")
        , Url.map Sold (s "sold")
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
