module Request.OnSaleProperty exposing (all, allGraphQl)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization, authorizationHeaders)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty, graphQlSpec)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)


all : Maybe AuthToken -> Http.Request (List OnSaleProperty)
all maybeToken =
    apiUrl ("/on-sale")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list OnSaleProperty.decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest


onSalePropertiesQuery : Request Query (List OnSaleProperty)
onSalePropertiesQuery =
    extract
        (field "onSaleProperties"
            []
            (list OnSaleProperty.graphQlSpec)
        )
        |> queryDocument
        |> request ()


allGraphQl : Maybe AuthToken -> Task GraphQLClient.Error (List OnSaleProperty)
allGraphQl maybeToken =
    GraphQLClient.customSendQuery
        { method = "POST"
        , headers = authorizationHeaders maybeToken
        , url = apiUrl "/graphql"
        , timeout = Nothing
        , withCredentials = False
        }
        onSalePropertiesQuery
