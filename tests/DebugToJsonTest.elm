module DebugToJsonTest exposing (..)

import DebugToJson
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The DebugToJson module"
        [ describe "format numbers"
            -- Nest as many descriptions as you like.
            [ test "()" <|
                \_ ->
                    "()"
                        |> DebugToJson.pp
                        |> Expect.equal "[]"
            , test "hex" <|
                \_ ->
                    "{asdf = \"qwer\"}"
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"asdf\": \"qwer\"\n}"
            ]
        ]
