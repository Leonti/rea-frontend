module Data.Session exposing (Session, attempt)

import Data.AuthToken exposing (AuthToken)


type alias Session =
    { maybeAuthToken : Maybe AuthToken }


attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    case session.maybeAuthToken of
        Nothing ->
            ( [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ], Cmd.none )

        Just token ->
            ( [], toCmd token )
