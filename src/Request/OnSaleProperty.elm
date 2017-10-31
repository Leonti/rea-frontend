module Request.OnSaleProperty exposing (all)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)


all : Maybe AuthToken -> Http.Request (List OnSaleProperty)
all maybeToken =
    apiUrl ("/on-sale")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list OnSaleProperty.decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest
