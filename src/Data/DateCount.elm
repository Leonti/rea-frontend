module Data.DateCount exposing (DateCount, graphQlSpec)

import GraphQL.Request.Builder exposing (..)
import Time.Date as LocalDate
import Json.Decode as Decode


type alias DateCount =
    { date : LocalDate.Date
    , count : Int
    }


graphQlSpec : ValueSpec NonNull ObjectType DateCount vars
graphQlSpec =
    object DateCount
        |> with (field "date" [] date)
        |> with (field "count" [] int)


type DateType
    = DateType


date : ValueSpec NonNull DateType LocalDate.Date vars
date =
    Decode.string
        |> Decode.andThen
            (\dateString ->
                case LocalDate.fromISO8601 dateString of
                    Ok date ->
                        Decode.succeed date

                    Err errorMessage ->
                        Decode.fail errorMessage
            )
        |> customScalar DateType
