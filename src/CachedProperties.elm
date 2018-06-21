module CachedProperties
    exposing
        ( CachedOnSaleState
        , CachedDateCountsState
        , OnSaleMsg
        , DateCountsMsg
        , initialOnSaleState
        , updateOnSale
        , initOrUpdateOnSale
        , updateDateCounts
        , initOrUpdateDateCounts
        , initialDateCountsState
        , onSale
        , dateCounts
        )

import Data.Session as Session exposing (Session)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Data.DateCount as DateCount exposing (DateCount)
import Time.Date as LocalDate
import Request.OnSaleProperty as OnSalePropertyRequest
import Request.DateCount as DateCountRequest
import Task exposing (Task)
import Debug
import GraphQL.Client.Http as GraphQLClient


type alias CachedOnSaleState =
    LoadingState (List OnSaleProperty)


type alias CachedDateCountsState =
    LoadingState (List DateCount)


type LoadingState a
    = NotLoaded
    | Loading
    | Loaded a
    | Failed String


type OnSaleMsg
    = OnSaleResult (Result (List String) (List OnSaleProperty))


type DateCountsMsg
    = DateCountsResult (Result (List String) (List DateCount))


initialOnSaleState : CachedOnSaleState
initialOnSaleState =
    NotLoaded


initialDateCountsState : CachedDateCountsState
initialDateCountsState =
    NotLoaded


initOrUpdateOnSale : Session -> Maybe LocalDate.Date -> Cmd OnSaleMsg
initOrUpdateOnSale session maybeDate =
    case ( session.maybeAuthToken, maybeDate ) of
        ( Just authToken, Just date ) ->
            let
                loadOnSaleProperties =
                    OnSalePropertyRequest.forDateGraphQl date authToken
            in
                Task.attempt OnSaleResult <| Task.mapError transformError loadOnSaleProperties

        _ ->
            Cmd.none


initOrUpdateDateCounts : CachedDateCountsState -> Session -> ( CachedDateCountsState, Cmd DateCountsMsg )
initOrUpdateDateCounts state session =
    case state of
        Loading ->
            ( state, Cmd.none )

        Loaded _ ->
            ( state, Cmd.none )

        _ ->
            let
                loadDateCounts =
                    DateCountRequest.allGraphQl session.maybeAuthToken
            in
                ( Loading, Task.attempt DateCountsResult <| Task.mapError transformError loadDateCounts )


transformError : GraphQLClient.Error -> List String
transformError error =
    [ Debug.log "graphql error" <| toString error ]


onSale : CachedOnSaleState -> Maybe (List OnSaleProperty)
onSale state =
    case state of
        Loaded onSalePropertyList ->
            Just onSalePropertyList

        _ ->
            Nothing


dateCounts : CachedDateCountsState -> Maybe (List DateCount)
dateCounts state =
    case state of
        Loaded dateCounts ->
            Just dateCounts

        _ ->
            Nothing


updateOnSale : CachedOnSaleState -> OnSaleMsg -> CachedOnSaleState
updateOnSale state msg =
    case msg of
        OnSaleResult (Ok onSale) ->
            Loaded onSale

        OnSaleResult (Err _) ->
            Failed "Failed to load on sale properties"


updateDateCounts : CachedDateCountsState -> DateCountsMsg -> CachedDateCountsState
updateDateCounts state msg =
    case msg of
        DateCountsResult (Ok dateCounts) ->
            Loaded dateCounts

        DateCountsResult (Err _) ->
            Failed "Failed to load on sale properties"
