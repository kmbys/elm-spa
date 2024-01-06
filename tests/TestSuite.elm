module TestSuite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Url exposing (Url)
import Route exposing (Route)

testParse : String -> String -> Maybe Route -> Test
testParse name input expected =
    test name <|
        \_ ->
            Url.fromString ("http://example.com" ++ input)
                |> Maybe.andThen Route.parse
                |> Expect.equal expected

suite : Test
suite =
    describe "Route"
        [ testParse "should parse URL" "/" (Just Route.Top)
        ]
