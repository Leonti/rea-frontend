module Page.Sold exposing (Model, init, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Data.SoldProperty as SoldProperty exposing (SoldProperty)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.SoldProperty
import Task exposing (Task)
import Views.Page as Page


-- MODEL --


type alias Model =
    { soldProperties : List SoldProperty
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadSoldProperties =
            Request.SoldProperty.all session.maybeAuthToken
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.Sold "Could not load sold properties"
    in
        Task.map Model loadSoldProperties
            |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html msg
view session model =
    div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-3" ]
                    [ div [ class "sidebar" ]
                        [ p [] [ text "Popular Tags" ]
                        , viewSoldProperties model.soldProperties
                        ]
                    ]
                ]
            ]
        ]


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


viewSoldProperties : List SoldProperty -> Html msg
viewSoldProperties soldProperties =
    div [ class "tag-list" ] (List.map viewSoldProperty soldProperties)


viewSoldProperty : SoldProperty -> Html msg
viewSoldProperty soldProperty =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        ]
        [ text (soldProperty.location) ]
