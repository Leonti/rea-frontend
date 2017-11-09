module Data.AuthToken exposing (AuthToken, decoder, encode, withAuthorization, extractAccessToken)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Dict


type alias AuthToken =
    { token : String
    , expiresAt : Int
    }


encode : AuthToken -> Value
encode authToken =
    Encode.object
        [ ( "token", Encode.string authToken.token )
        , ( "expiresAt", Encode.int authToken.expiresAt )
        ]


decoder : Decoder AuthToken
decoder =
    decode AuthToken
        |> required "token" Decode.string
        |> required "expiresAt" Decode.int


withAuthorization : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeToken builder =
    case maybeToken of
        Just authToken ->
            builder
                |> withHeader "authorization" ("Bearer " ++ authToken.token)

        Nothing ->
            builder


extractAccessToken : String -> Maybe ( String, Int )
extractAccessToken hash =
    let
        eachParam =
            (String.split "&" hash)

        eachPair =
            List.map (splitAtFirst '=') eachParam

        allParams =
            Dict.fromList eachPair

        maybeToken =
            Dict.get "access_token" allParams

        maybeExpires =
            Dict.get "expires_at" allParams
    in
        Maybe.map2 (\token expiresAt -> ( token, (Result.withDefault 0 (String.toInt expiresAt)) )) maybeToken maybeExpires


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
