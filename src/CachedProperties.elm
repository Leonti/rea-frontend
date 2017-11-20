module CachedProperties exposing (State, initialState, init, update, Msg)

import Data.Session as Session exposing (Session)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Data.SoldProperty as SoldProperty exposing (SoldProperty)
import Request.OnSaleProperty
import Task exposing (Task)
import Http


type alias State =
    { onSale : LoadingState (List OnSaleProperty)
    , sold : LoadingState (List SoldProperty)
    }


type LoadingState a
    = NotLoaded
    | Loading
    | Loaded a
    | Failed String


type Msg
    = OnSaleResult (Result Http.Error (List OnSaleProperty))
    | SoldResult (Result Http.Error (List SoldProperty))


initialState : State
initialState =
    { onSale = NotLoaded, sold = NotLoaded }


init : Session -> ( State, Cmd Msg )
init session =
    let
        loadOnSaleProperties =
            Request.OnSaleProperty.all session.maybeAuthToken
                |> Http.toTask

        state =
            { onSale = Loading
            , sold = Loading
            }
    in
        ( state, Task.attempt OnSaleResult loadOnSaleProperties )


update : State -> Msg -> State
update state msg =
    case msg of
        OnSaleResult (Ok onSale) ->
            { state | onSale = Loaded onSale }

        OnSaleResult (Err _) ->
            { state | onSale = Failed "Failed to load on sale properties" }

        SoldResult (Ok sold) ->
            { state | sold = Loaded sold }

        SoldResult (Err _) ->
            { state | sold = Failed "Failed to load sold properties" }
