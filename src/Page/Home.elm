module Page.Home exposing (view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Route as Route


-- VIEW --


view : Session -> Html msg
view session =
    div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-3" ]
                    [ div []
                        [ a [ Route.href Route.OnSale ] [ text "Properties on sale" ]
                        ]
                    ]
                ]
            ]
        ]


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "Melbourne Properties" ]
            , p [] [ text "Melbourne Properties tracker" ]
            ]
        ]
