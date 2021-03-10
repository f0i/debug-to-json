module DebugToJsonTest exposing (..)

import Array exposing (Array)
import DebugToJson
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Process
import Set exposing (Set)
import Task exposing (Task)
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
            , test "function" <|
                \_ ->
                    max
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "\"<function>\""
            , test "internal" <|
                \_ ->
                    Process.sleep 0.1
                        |> Debug.toString
                        |> DebugToJson.pp
                        |> Expect.equal "\"<internals>\""
            , test "model" <|
                \_ ->
                    """
{ aggregates = Dict.fromList [("{"DasTool":0}",Dict.fromList [(3,{ data = Failure NetworkError, scale = "hour" })]),("{"Importer":0}",Dict.fromList [(1,{ data = Failure NetworkError, scale = "hour" }),(2,{ data = Failure NetworkError, scale = "hour" }),(3,{ data = Failure NetworkError, scale = "hour" }),(4,{ data = Failure NetworkError, scale = "hour" })])], chartState = { drag = Hover, hover = Nothing, mark = Nothing, tempMark = Nothing }, command = "0123456789ABCDEF", commandLog = [<internals>,<internals>,<internals>,<internals>,<internals>], dieBox = Failure NetworkError, history = Failure NetworkError, inputs = Dict.fromList [(1,{ filter = [<function>], lastValid = "100", validators = [<function>,<function>], value = "100" }),(2,{ filter = [<function>,<function>], lastValid = "01", validators = [<function>], value = "01" }),(3,{ filter = [<function>,<function>], lastValid = "01A", validators = [<function>], value = "01A" }),(4,{ filter = [<function>,<function>,<function>,<function>], lastValid = "", validators = [<function>], value = "" }),(5,{ filter = [], lastValid = "http://localhost:8088/api", validators = [<function>], value = "http://localhost:8088/api" }),(6,{ filter = [], lastValid = "ws://localhost:8088/ws", validators = [<function>], value = "ws://localhost:8088/ws" }),(7,{ filter = [<function>], lastValid = "10", validators = [<function>], value = "10" }),(8,{ filter = [], lastValid = "local,", validators = [<function>], value = "local," }),(9,{ filter = [], lastValid = "example@example.onion", validators = [<function>], value = "example@example.onion" }),(10,{ filter = [], lastValid = "smtp.example.onion:25", validators = [<function>], value = "smtp.example.onion:25" }),(11,{ filter = [], lastValid = "user", validators = [<function>], value = "user" }),(12,{ filter = [], lastValid = "******", validators = [<function>], value = "******" }),(13,{ filter = [], lastValid = "AKIAIOSFODNN7EXAMPLE", validators = [<function>], value = "AKIAIOSFODNN7EXAMPLE" }),(14,{ filter = [], lastValid = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY", validators = [<function>], value = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" }),(15,{ filter = [], lastValid = "s3.amazonaws.com/bucket_name", validators = [<function>], value = "s3.amazonaws.com/bucket_name" }),(16,{ filter = [<function>,<function>], lastValid = "01", validators = [<function>], value = "01" }),(17,{ filter = [<function>], lastValid = "0", validators = [<function>], value = "0" }),(18,{ filter = [<function>], lastValid = "0", validators = [<function>], value = "0" }),(19,{ filter = [<function>,<function>], lastValid = "00", validators = [<function>], value = "00" }),(20,{ filter = [<function>,<function>], lastValid = "00 01 02 03 04 05 06 comments", validators = [<function>], value = "00 01 02 03 04 05 06 comments" })], lang = DE, lastTick = Posix 1615360785547, navKey = <function>, storage = [], url = { fragment = Nothing, host = "127.0.0.1", path = "/Insights", port_ = Just 8000, protocol = Http, query = Nothing }, user = Just "Guest", width = 1916 }
"""
                        |> String.trim
                        |> DebugToJson.pp
                        |> Expect.equal "{\n    \"aggregates\": {\n        \"{\": {\n            \"Importer\": [\n                \":0}\",\n                {\n                    \"1\": {\n                        \"data\": {\n                            \"Failure\": [\n                                \"NetworkError\"\n                            ]\n                        },\n                        \"scale\": \"hour\"\n                    },\n                    \"2\": {\n                        \"data\": {\n                            \"Failure\": [\n                                \"NetworkError\"\n                            ]\n                        },\n                        \"scale\": \"hour\"\n                    },\n                    \"3\": {\n                        \"data\": {\n                            \"Failure\": [\n                                \"NetworkError\"\n                            ]\n                        },\n                        \"scale\": \"hour\"\n                    },\n                    \"4\": {\n                        \"data\": {\n                            \"Failure\": [\n                                \"NetworkError\"\n                            ]\n                        },\n                        \"scale\": \"hour\"\n                    }\n                }\n            ]\n        }\n    },\n    \"chartState\": {\n        \"drag\": \"Hover\",\n        \"hover\": \"Nothing\",\n        \"mark\": \"Nothing\",\n        \"tempMark\": \"Nothing\"\n    },\n    \"command\": \"0123456789ABCDEF\",\n    \"commandLog\": [\n        \"<internals>\",\n        \"<internals>\",\n        \"<internals>\",\n        \"<internals>\",\n        \"<internals>\"\n    ],\n    \"dieBox\": {\n        \"Failure\": [\n            \"NetworkError\"\n        ]\n    },\n    \"history\": {\n        \"Failure\": [\n            \"NetworkError\"\n        ]\n    },\n    \"inputs\": {\n        \"1\": {\n            \"filter\": [\n                \"<function>\"\n            ],\n            \"lastValid\": \"100\",\n            \"validators\": [\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"value\": \"100\"\n        },\n        \"2\": {\n            \"filter\": [\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"lastValid\": \"01\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"01\"\n        },\n        \"3\": {\n            \"filter\": [\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"lastValid\": \"01A\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"01A\"\n        },\n        \"4\": {\n            \"filter\": [\n                \"<function>\",\n                \"<function>\",\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"lastValid\": \"\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"\"\n        },\n        \"5\": {\n            \"filter\": [],\n            \"lastValid\": \"http://localhost:8088/api\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"http://localhost:8088/api\"\n        },\n        \"6\": {\n            \"filter\": [],\n            \"lastValid\": \"ws://localhost:8088/ws\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"ws://localhost:8088/ws\"\n        },\n        \"7\": {\n            \"filter\": [\n                \"<function>\"\n            ],\n            \"lastValid\": \"10\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"10\"\n        },\n        \"8\": {\n            \"filter\": [],\n            \"lastValid\": \"local,\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"local,\"\n        },\n        \"9\": {\n            \"filter\": [],\n            \"lastValid\": \"example@example.onion\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"example@example.onion\"\n        },\n        \"10\": {\n            \"filter\": [],\n            \"lastValid\": \"smtp.example.onion:25\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"smtp.example.onion:25\"\n        },\n        \"11\": {\n            \"filter\": [],\n            \"lastValid\": \"user\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"user\"\n        },\n        \"12\": {\n            \"filter\": [],\n            \"lastValid\": \"******\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"******\"\n        },\n        \"13\": {\n            \"filter\": [],\n            \"lastValid\": \"AKIAIOSFODNN7EXAMPLE\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"AKIAIOSFODNN7EXAMPLE\"\n        },\n        \"14\": {\n            \"filter\": [],\n            \"lastValid\": \"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\"\n        },\n        \"15\": {\n            \"filter\": [],\n            \"lastValid\": \"s3.amazonaws.com/bucket_name\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"s3.amazonaws.com/bucket_name\"\n        },\n        \"16\": {\n            \"filter\": [\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"lastValid\": \"01\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"01\"\n        },\n        \"17\": {\n            \"filter\": [\n                \"<function>\"\n            ],\n            \"lastValid\": \"0\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"0\"\n        },\n        \"18\": {\n            \"filter\": [\n                \"<function>\"\n            ],\n            \"lastValid\": \"0\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"0\"\n        },\n        \"19\": {\n            \"filter\": [\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"lastValid\": \"00\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"00\"\n        },\n        \"20\": {\n            \"filter\": [\n                \"<function>\",\n                \"<function>\"\n            ],\n            \"lastValid\": \"00 01 02 03 04 05 06 comments\",\n            \"validators\": [\n                \"<function>\"\n            ],\n            \"value\": \"00 01 02 03 04 05 06 comments\"\n        }\n    },\n    \"lang\": \"DE\",\n    \"lastTick\": {\n        \"Posix\": [\n            1615360785547\n        ]\n    },\n    \"navKey\": \"<function>\",\n    \"storage\": [],\n    \"url\": {\n        \"fragment\": \"Nothing\",\n        \"host\": \"127.0.0.1\",\n        \"path\": \"/Insights\",\n        \"port_\": {\n            \"Just\": [\n                8000\n            ]\n        },\n        \"protocol\": \"Http\",\n        \"query\": \"Nothing\"\n    },\n    \"user\": {\n        \"Just\": [\n            \"Guest\"\n        ]\n    },\n    \"width\": 1916\n}"
            ]
        ]
