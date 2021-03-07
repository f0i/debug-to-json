module DebugToJsonTest exposing (..)

import DebugToJson
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


type MyType
    = MyFirst String Int
    | MySecond


suite : Test
suite =
    describe "The DebugToJson module"
        [ describe "format objects"
            -- Nest as many descriptions as you like.
            [ test "()" <|
                \_ ->
                    ()
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "[]"
            , test "hash" <|
                \_ ->
                    { asdf = "qwer" }
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"asdf\": \"qwer\"\n}"
            , test "custom type 1" <|
                \_ ->
                    MyFirst "asdf" 123
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"MyFirst\": [\n        \"asdf\",\n        123\n    ]\n}"
            , test "custom type 2" <|
                \_ ->
                    MySecond
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"MySecond\": []\n}"
            , test "float" <|
                \_ ->
                    12.34
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "12.34"
            , test "negative float" <|
                \_ ->
                    -12.34
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "-12.34"
            ]
        ]
