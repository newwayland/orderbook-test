module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    test "the empty list has 0 length" <|
        \_ ->
            List.length []
                |> Expect.equal 0
