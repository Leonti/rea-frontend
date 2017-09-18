module Data.AuthToken exposing (AuthToken(..), decoder, encode, withAuthorization, extractAccessToken)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Dict


type AuthToken
    = AuthToken String


encode : AuthToken -> Value
encode (AuthToken token) =
    Encode.string token


decoder : Decoder AuthToken
decoder =
    Decode.string
        |> Decode.map AuthToken


withAuthorization : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeToken builder =
    case maybeToken of
        Just (AuthToken token) ->
            builder
                |> withHeader "authorization" ("Token " ++ token)

        Nothing ->
            builder


extractAccessToken : String -> Maybe String
extractAccessToken hash =
    let
        eachParam =
            (String.split "&" hash)

        eachPair =
            List.map (splitAtFirst '=') eachParam

        allParams =
            Dict.fromList eachPair
    in
        Dict.get "access_token" allParams


splitAtFirst : Char -> String -> ( String, String )
splitAtFirst c s =
    case (firstOccurrence c s) of
        Nothing ->
            ( s, "" )

        Just i ->
            ( (String.left i s), (String.dropLeft (i + 1) s) )


firstOccurrence : Char -> String -> Maybe Int
firstOccurrence c s =
    case (String.indexes (String.fromChar c) s) of
        [] ->
            Nothing

        head :: _ ->
            Just head
