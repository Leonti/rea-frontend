module Request.OnSaleProperty exposing (forDateGraphQl)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization, authorizationHeaders)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty, graphQlSpec)
import Request.Helpers exposing (apiUrl)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)
import Time.Date as LocalDate exposing (year, month, day)
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Request.Builder.Arg as Arg


onSalePropertiesQuery : Document Query (List OnSaleProperty) { vars | date : String }
onSalePropertiesQuery =
    let
        dateVar =
            Var.required "date" .date Var.string

        queryRoot =
            extract
                (field "onSalePropertiesForDate"
                    [ ( "date", Arg.variable dateVar ) ]
                    (list OnSaleProperty.graphQlSpec)
                )
    in
        queryDocument queryRoot


dateToString : LocalDate.Date -> String
dateToString date =
    (toString <| year date) ++ "-" ++ (toString <| month date) ++ "-" ++ (toString <| day date)


onSalePropertiesQueryRequest : LocalDate.Date -> Request Query (List OnSaleProperty)
onSalePropertiesQueryRequest date =
    onSalePropertiesQuery |> request { date = dateToString date }


forDateGraphQl : LocalDate.Date -> AuthToken -> Task GraphQLClient.Error (List OnSaleProperty)
forDateGraphQl date token =
    GraphQLClient.customSendQuery
        { method = "POST"
        , headers = authorizationHeaders (Just token)
        , url = apiUrl "/graphql"
        , timeout = Nothing
        , withCredentials = False
        }
    <|
        onSalePropertiesQueryRequest date
