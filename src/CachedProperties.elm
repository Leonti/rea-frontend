module CachedProperties
    exposing
        ( CachedOnSaleState
        , CachedSoldState
        , OnSaleMsg
        , SoldMsg
        , initialOnSaleState
        , initialSoldState
        , updateOnSale
        , updateSold
        , initOrUpdateOnSale
        , onSale
        )

import Data.Session as Session exposing (Session)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Data.SoldProperty as SoldProperty exposing (SoldProperty)
import Request.OnSaleProperty as OnSalePropertyRequest
import Task exposing (Task)
import Debug
import GraphQL.Client.Http as GraphQLClient


type alias CachedOnSaleState =
    LoadingState (List OnSaleProperty)


type alias CachedSoldState =
    LoadingState (List SoldProperty)


type LoadingState a
    = NotLoaded
    | Loading
    | Loaded a
    | Failed String


type OnSaleMsg
    = OnSaleResult (Result (List String) (List OnSaleProperty))


type SoldMsg
    = SoldResult (Result (List String) (List SoldProperty))


initialOnSaleState : CachedOnSaleState
initialOnSaleState =
    NotLoaded


initialSoldState : CachedSoldState
initialSoldState =
    NotLoaded


initOrUpdateOnSale : CachedOnSaleState -> Session -> ( CachedOnSaleState, Cmd OnSaleMsg )
initOrUpdateOnSale state session =
    case state of
        Loading ->
            ( state, Cmd.none )

        Loaded _ ->
            ( state, Cmd.none )

        _ ->
            let
                loadOnSaleProperties =
                    OnSalePropertyRequest.allGraphQl session.maybeAuthToken
            in
                ( Loading, Task.attempt OnSaleResult <| Task.mapError transformError loadOnSaleProperties )


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


updateOnSale : CachedOnSaleState -> OnSaleMsg -> CachedOnSaleState
updateOnSale state msg =
    case msg of
        OnSaleResult (Ok onSale) ->
            Loaded onSale

        OnSaleResult (Err _) ->
            Failed "Failed to load on sale properties"


updateSold : CachedSoldState -> SoldMsg -> CachedSoldState
updateSold state msg =
    case msg of
        SoldResult (Ok sold) ->
            Loaded sold

        SoldResult (Err _) ->
            Failed "Failed to load sold properties"
