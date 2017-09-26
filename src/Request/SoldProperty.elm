module Request.SoldProperty exposing (all)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.SoldProperty as SoldProperty exposing (SoldProperty)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)


all : Maybe AuthToken -> Http.Request (List SoldProperty)
all maybeToken =
    apiUrl ("/sold")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list SoldProperty.decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest
