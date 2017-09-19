module Data.Storage exposing (Storage, decoder, encode, store)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Ports


type alias Storage =
    { token : Maybe AuthToken
    }



-- SERIALIZATION --


decoder : Decoder Storage
decoder =
    decode Storage
        |> required "token" (Decode.nullable AuthToken.decoder)


encode : Storage -> Value
encode storage =
    Encode.object
        [ ( "token", EncodeExtra.maybe AuthToken.encode storage.token )
        ]


store : Storage -> Cmd msg
store storage =
    encode storage
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
