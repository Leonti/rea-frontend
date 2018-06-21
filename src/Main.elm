module Main exposing (main)

import Html exposing (..)
import Data.Session as Session exposing (Session)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Sold as Sold
import Page.OnSale as OnSale
import Page.NotFound as NotFound
import CachedProperties as CachedProperties
import Route exposing (Route)
import Task
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Views.Page as Page exposing (ActivePage)
import Data.Storage as Storage exposing (Storage)
import Task
import Time exposing (Time)


-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home
    | Sold Sold.Model
    | OnSale OnSale.Model
    | Login


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --


type alias Model =
    { session : Session
    , pageState : PageState
    , cachedOnSaleState : CachedProperties.CachedOnSaleState
    , cachedDateCountsState : CachedProperties.CachedDateCountsState
    , currentRoute : Maybe Route
    , currentTime : Maybe Time
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        storage =
            decodeStorageFromJson val
    in
        ( { pageState = Loaded initialPage
          , cachedOnSaleState = CachedProperties.initialOnSaleState
          , cachedDateCountsState = CachedProperties.initialDateCountsState
          , session =
                { maybeAuthToken = Maybe.andThen .token storage
                , maybeExpiresAt = Maybe.andThen .expiresAt storage
                }
          , currentRoute = Route.fromLocation location
          , currentTime = Nothing
          }
        , Task.perform InitialTime Time.now
        )



--        setRoute (Route.fromLocation location)


decodeStorageFromJson : Value -> Maybe Storage
decodeStorageFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString Storage.decoder >> Result.toMaybe)


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model False page

        TransitioningFrom page ->
            viewPage model True page


isValidAuthentication : Model -> Bool
isValidAuthentication model =
    let
        maybeIsValid =
            Maybe.map3 (\_ expiresAt currentTime -> (currentTime + 300) < expiresAt) model.session.maybeAuthToken model.session.maybeExpiresAt (Maybe.map inSeconds model.currentTime)
    in
        Maybe.withDefault False maybeIsValid


viewPage : Model -> Bool -> Page -> Html Msg
viewPage model isLoading page =
    let
        frame =
            Page.frame isLoading (isValidAuthentication model)
    in
        case page of
            NotFound ->
                NotFound.view
                    |> frame Page.Other

            Blank ->
                -- This is for the very initial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame Page.Other

            Login ->
                Html.text ""
                    |> frame Page.Other

            Home ->
                Home.view (isValidAuthentication model)
                    |> frame Page.Home

            Errored subModel ->
                Errored.view model.session subModel
                    |> frame Page.Other

            Sold subModel ->
                Sold.view model.session subModel
                    |> frame Page.Sold

            OnSale subModel ->
                OnSale.view model.session (Maybe.withDefault 0.0 model.currentTime) (CachedProperties.dateCounts model.cachedDateCountsState) (CachedProperties.onSale model.cachedOnSaleState) subModel
                    |> frame Page.OnSale
                    |> Html.map OnSaleMsg



-- SUBSCRIPTIONS --
-- Note: we aren't currently doing any page subscriptions, but I thought it would
-- be a good idea to put this in here as an example. If I were actually
-- maintaining this in production, I wouldn't bother until I needed this!


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
        , Time.every (Time.second * 5) CurrentTime
          --        , Sub.map SetUser sessionChange
        ]



--sessionChange : Sub (Maybe User)
--sessionChange =
--    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        Login ->
            Sub.none

        Home ->
            Sub.none

        NotFound ->
            Sub.none

        Sold _ ->
            Sub.none

        OnSale subModel ->
            OnSale.subscriptions subModel |> Sub.map OnSaleMsg



-- UPDATE --


type Msg
    = InitialTime Time
    | CurrentTime Time
    | SetRoute (Maybe Route)
    | SoldLoaded (Result PageLoadError Sold.Model)
    | OnSaleLoaded (Result PageLoadError OnSale.Model)
    | OnSaleMsg OnSale.Msg
    | CachedPropertiesOnSaleMsg CachedProperties.OnSaleMsg
    | CachedPropertiesDateCountsMsg CachedProperties.DateCountsMsg
    | CachedPropertiesLoaded (List OnSaleProperty)


inSeconds : Time -> Int
inSeconds t =
    Time.inSeconds t |> round


setRoute : Model -> ( Model, Cmd Msg )
setRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getPage model.pageState) }
            , Task.attempt toMsg task
            )

        errored =
            pageErrored model
    in
        case model.currentRoute of
            Nothing ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            Just (Route.Home (Just authToken)) ->
                let
                    session =
                        model.session

                    maybeExpiresAt =
                        Maybe.map (\t -> (inSeconds t) + authToken.expiresIn) model.currentTime

                    updatedSession : Session
                    updatedSession =
                        { session
                            | maybeAuthToken = Just authToken
                            , maybeExpiresAt = maybeExpiresAt
                        }
                in
                    ( { model
                        | pageState =
                            TransitioningFrom (getPage model.pageState)
                        , session = updatedSession
                      }
                    , Cmd.batch
                        [ Storage.store
                            { token = Just authToken
                            , expiresAt = maybeExpiresAt
                            }
                        , Route.modifyUrl (Route.Home Nothing)
                        ]
                    )

            Just (Route.Home Nothing) ->
                ( { model | pageState = Loaded Home }, Cmd.none )

            Just (Route.Login) ->
                ( { model | pageState = Loaded Login }, Cmd.none )

            Just (Route.Sold) ->
                transition SoldLoaded (Sold.init model.session)

            Just (Route.OnSale) ->
                let
                    ( cachedDateCountsState, cachedDateCountsMsg ) =
                        CachedProperties.initOrUpdateDateCounts model.cachedDateCountsState model.session
                in
                    ( { model
                        | cachedDateCountsState = cachedDateCountsState
                        , pageState = Loaded (OnSale <| OnSale.initialModel Nothing Nothing)
                      }
                    , Cmd.map CachedPropertiesDateCountsMsg cachedDateCountsMsg
                    )

            --                    transition OnSaleLoaded (OnSale.init model.session (Maybe.withDefault 0.0 model.currentTime))
            Just (Route.OnSaleForDate date) ->
                let
                    ( cachedDateCountsState, cachedDateCountsMsg ) =
                        CachedProperties.initOrUpdateDateCounts model.cachedDateCountsState model.session

                    cachedPropertiesCmd =
                        CachedProperties.initOrUpdateOnSale model.session (Just date)
                in
                    ( { model
                        | cachedDateCountsState = cachedDateCountsState
                        , pageState = Loaded (OnSale <| OnSale.initialModel (Just date) Nothing)
                      }
                    , Cmd.batch
                        [ Cmd.map CachedPropertiesDateCountsMsg cachedDateCountsMsg
                        , Cmd.map CachedPropertiesOnSaleMsg cachedPropertiesCmd
                        ]
                    )

            Just (Route.OnSaleForDateExpanded maybeDate propertyId) ->
                let
                    ( cachedDateCountsState, cachedDateCountsMsg ) =
                        CachedProperties.initOrUpdateDateCounts model.cachedDateCountsState model.session

                    cachedPropertiesCmd =
                        CachedProperties.initOrUpdateOnSale model.session maybeDate
                in
                    ( { model
                        | cachedDateCountsState = cachedDateCountsState
                        , pageState = Loaded (OnSale <| OnSale.initialModel maybeDate (Just propertyId))
                      }
                    , Cmd.batch
                        [ Cmd.map CachedPropertiesDateCountsMsg cachedDateCountsMsg
                        , Cmd.map CachedPropertiesOnSaleMsg cachedPropertiesCmd
                        ]
                    )



-- transition OnSaleLoaded (OnSale.init model.session (Maybe.withDefault 0.0 model.currentTime))


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        ( { model | pageState = Loaded (Errored error) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
        case ( msg, page ) of
            ( InitialTime currentTime, _ ) ->
                setRoute { model | currentTime = Just currentTime }

            ( CurrentTime currentTime, _ ) ->
                ( { model | currentTime = Just currentTime }, Cmd.none )

            ( CachedPropertiesDateCountsMsg msg, _ ) ->
                ( { model | cachedDateCountsState = CachedProperties.updateDateCounts model.cachedDateCountsState msg }
                , Cmd.none
                )

            ( CachedPropertiesOnSaleMsg msg, _ ) ->
                ( { model | cachedOnSaleState = CachedProperties.updateOnSale model.cachedOnSaleState msg }
                , Cmd.none
                )

            ( SetRoute route, _ ) ->
                setRoute { model | currentRoute = route }

            ( SoldLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Sold subModel) }, Cmd.none )

            ( SoldLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Errored error) }, Cmd.none )

            ( OnSaleLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (OnSale subModel) }, Cmd.none )

            ( OnSaleLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Errored error) }, Cmd.none )

            ( OnSaleMsg subMsg, OnSale subModel ) ->
                toPage OnSale OnSaleMsg (OnSale.update session) subMsg subModel

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                ( model, Cmd.none )

            -- Disregard incoming messages that arrived for the wrong page
            ( _, _ ) ->
                ( model, Cmd.none )



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
