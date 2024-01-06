module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, string, top, oneOf, (</>))

type Route
    = Top
    | User String
    | Repo String String

parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        , map User string
        , map Repo (string </> string)
        ]

fromUrl : Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url
