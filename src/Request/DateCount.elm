module Request.DateCount exposing (allGraphQl)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization, authorizationHeaders)
import Data.DateCount as DateCount exposing (DateCount, graphQlSpec)
import Request.Helpers exposing (apiUrl)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)


dateCountsQuery : Request Query (List DateCount)
dateCountsQuery =
    extract
        (field "onSaleDates"
            []
            (list DateCount.graphQlSpec)
        )
        |> queryDocument
        |> request ()


allGraphQl : Maybe AuthToken -> Task GraphQLClient.Error (List DateCount)
allGraphQl maybeToken =
    GraphQLClient.customSendQuery
        { method = "POST"
        , headers = authorizationHeaders maybeToken
        , url = apiUrl "/graphql"
        , timeout = Nothing
        , withCredentials = False
        }
        dateCountsQuery
