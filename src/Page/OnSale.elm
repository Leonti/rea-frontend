module Page.OnSale exposing (Model, init, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Data.OnSaleProperty as OnSaleProperty exposing (OnSaleProperty)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.OnSaleProperty
import Task exposing (Task)
import Views.Page as Page


-- MODEL --


type alias Model =
    { onSaleProperties : List OnSaleProperty
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadOnSaleProperties =
            Request.OnSaleProperty.all session.maybeAuthToken
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.OnSale "Could not load on sale properties"
    in
        Task.map Model loadOnSaleProperties
            |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html msg
view session model =
    div [ class "home-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-3" ]
                    [ div [ class "sidebar" ]
                        [ viewOnSaleProperties model.onSaleProperties
                        ]
                    ]
                ]
            ]
        ]


viewOnSaleProperties : List OnSaleProperty -> Html msg
viewOnSaleProperties onSaleProperties =
    div [ class "tag-list" ] (List.map viewOnSaleProperty onSaleProperties)


viewOnSaleProperty : OnSaleProperty -> Html msg
viewOnSaleProperty onSaleProperty =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        ]
        [ text (onSaleProperty.location) ]
