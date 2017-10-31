module Views.Page exposing (ActivePage(..), bodyId, frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Views.Spinner exposing (spinner)
import Request.Helpers exposing (loginUrl)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home
    | Sold
    | OnSale


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : Bool -> Bool -> ActivePage -> Html msg -> Html msg
frame isLoading isSignedIn page content =
    div [ class "page-frame" ]
        [ viewHeader isSignedIn page isLoading
        , content
        , viewFooter
        ]


viewHeader : Bool -> ActivePage -> Bool -> Html msg
viewHeader isSignedIn page isLoading =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href <| Route.Home Nothing ]
                [ text "Melbourne Apartments" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                (viewIf
                    isLoading
                    spinner
                )
                    :: navbarLink (page == Home) (Route.Home Nothing) [ text "Home" ]
                    :: (viewSignIn isSignedIn)
            ]
        ]


viewSignIn : Bool -> List (Html msg)
viewSignIn isSignedIn =
    if isSignedIn then
        []
    else
        [ li [ classList [ ( "nav-item", True ) ] ]
            [ a [ class "nav-link", href loginUrl ] [ text "Login" ] ]
        ]


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


viewFooter : Html msg
viewFooter =
    footer []
        []


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


{-| This id comes from index.html.

The Feed uses it to scroll to the top of the page (by ID) when switching pages
in the pagination sense.

-}
bodyId : String
bodyId =
    "page-body"
