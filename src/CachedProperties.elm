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
import Request.OnSaleProperty
import Task exposing (Task)
import Http


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
    = OnSaleResult (Result Http.Error (List OnSaleProperty))


type SoldMsg
    = SoldResult (Result Http.Error (List SoldProperty))


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
                    Request.OnSaleProperty.all session.maybeAuthToken
                        |> Http.toTask
            in
                ( Loading, Task.attempt OnSaleResult loadOnSaleProperties )


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
