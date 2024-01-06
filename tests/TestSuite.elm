module TestSuite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Url exposing (Url)
import Route exposing (Route)

testFromUrl : String -> String -> Maybe Route -> Test
testFromUrl name input expected =
    test name <|
        \_ ->
            Url.fromString ("http://example.com" ++ input)
                |> Maybe.andThen Route.fromUrl
                |> Expect.equal expected

suite : Test
suite =
    describe "Route"
        [ testFromUrl "should parse Top" "/" (Just Route.Top)
        , testFromUrl "should parse Top with query params" "/?dummy=value" (Just Route.Top)
        , testFromUrl "should parse Top with hash" "/#dummy" (Just Route.Top)
        , testFromUrl "should parse User" "/foo" (Just (Route.User "foo"))
        , testFromUrl "should parse Repo" "/foo/bar" (Just (Route.Repo "foo" "bar"))
        , testFromUrl "should parse invalid path" "/foo/bar/buz" Nothing
        ]
