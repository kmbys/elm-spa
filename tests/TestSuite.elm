module TestSuite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Url exposing (Url)
import Route


suite : Test
suite =
    describe "Route"
        [ test "should parse URL" <|
            \_ ->
                Url.fromString "http://example.com/"
                    |> Maybe.andThen Route.parse
                    |> Expect.equal (Just Route.Top)
        ]
