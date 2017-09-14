module Data.Article
    exposing
        ( Tag
        , tagDecoder
        , tagToString
        )

import Json.Decode as Decode exposing (Decoder)


-- TAGS --


type Tag
    = Tag String


tagToString : Tag -> String
tagToString (Tag slug) =
    slug


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map Tag Decode.string
