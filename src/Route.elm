module Route exposing (..)

import Url exposing (Url)

type Route
    = Top
    | User String
    | Repo String String

parse : Url -> Maybe Route
parse url =
    Just Top
