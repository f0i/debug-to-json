module DebugToJsonTest exposing (..)

import Array exposing (Array)
import DebugToJson
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set exposing (Set)
import Test exposing (..)


type MyType
    = MyFirst String Int
    | MySecond
    | DThird


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
                        |> Expect.equal "\"MySecond\""
            , test "custom type 3" <|
                \_ ->
                    DThird
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "\"DThird\""
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
            , test "negative int" <|
                \_ ->
                    -12
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "-12"
            , test "empty dict" <|
                \_ ->
                    Dict.fromList []
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "{}"
            , test "dict1" <|
                \_ ->
                    Dict.fromList [ ( "a", 1 ) ]
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"a\": 1\n}"
            , test "dict2" <|
                \_ ->
                    Dict.fromList [ ( "a", 1 ), ( "b", 2 ) ]
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"a\": 1,\n    \"b\": 2\n}"
            , test "Set" <|
                \_ ->
                    Set.fromList [ 1, 3, 2 ]
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "[\n    1,\n    2,\n    3\n]"
            , test "Array" <|
                \_ ->
                    Array.fromList [ 1, 3, 2 ]
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "[\n    1,\n    3,\n    2\n]"
            ]
        ]
