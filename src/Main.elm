module Main exposing (main)

import Html exposing (..)
import Data.Session as Session exposing (Session)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Sold as Sold
import Page.NotFound as NotFound
import Route exposing (Route)
import Task
import Views.Page as Page exposing (ActivePage)
import Data.Storage as Storage exposing (Storage)


-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home Home.Model
    | Sold Sold.Model
    | Login


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        storage =
            decodeStorageFromJson val
    in
        setRoute (Route.fromLocation location)
            { pageState = Loaded initialPage
            , session = { maybeAuthToken = Maybe.andThen .token storage }
            }


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
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            case session.maybeAuthToken of
                Just _ ->
                    Page.frame isLoading True

                Nothing ->
                    Page.frame isLoading False
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

            Errored subModel ->
                Errored.view session subModel
                    |> frame Page.Other

            Home subModel ->
                Home.view session subModel
                    |> frame Page.Home
                    |> Html.map HomeMsg

            Sold subModel ->
                Sold.view session subModel
                    |> frame Page.Sold



-- SUBSCRIPTIONS --
-- Note: we aren't currently doing any page subscriptions, but I thought it would
-- be a good idea to put this in here as an example. If I were actually
-- maintaining this in production, I wouldn't bother until I needed this!


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
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

        NotFound ->
            Sub.none

        Home _ ->
            Sub.none

        Sold _ ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Home.Model)
    | HomeMsg Home.Msg
    | SoldLoaded (Result PageLoadError Sold.Model)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getPage model.pageState) }
            , Task.attempt toMsg task
            )

        errored =
            pageErrored model
    in
        case maybeRoute of
            Nothing ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            Just (Route.Home (Just authToken)) ->
                let
                    session =
                        model.session

                    updatedSession : Session
                    updatedSession =
                        { session
                            | maybeAuthToken = Just authToken
                        }
                in
                    ( { model
                        | pageState =
                            TransitioningFrom (getPage model.pageState)
                        , session = updatedSession
                      }
                    , Cmd.batch
                        [ Task.attempt HomeLoaded (Home.init model.session)
                        , Storage.store { token = Just authToken }
                        ]
                    )

            Just (Route.Home Nothing) ->
                transition HomeLoaded (Home.init model.session)

            Just (Route.Login) ->
                ( { model | pageState = Loaded Login }, Cmd.none )

            Just (Route.Sold) ->
                transition SoldLoaded (Sold.init model.session)


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
            ( SetRoute route, _ ) ->
                setRoute route model

            ( HomeLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Home subModel) }, Cmd.none )

            ( HomeLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Errored error) }, Cmd.none )

            ( SoldLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Sold subModel) }, Cmd.none )

            ( SoldLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Errored error) }, Cmd.none )

            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg (Home.update session) subMsg subModel

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
