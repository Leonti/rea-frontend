module Request.Article
    exposing
        ( tags
        )

import Data.Article as Article exposing (Tag, tagDecoder)
import Http
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)


tags : Http.Request (List Tag)
tags =
    Decode.field "tags" (Decode.list Article.tagDecoder)
        |> Http.get (apiUrl "/tags")
