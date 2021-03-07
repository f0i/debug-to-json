module DebugToJson exposing
    ( pp
    , toJson
    )

{-| Convert Debug.toString output to JSON


# Pretty print

@docs pp


# Convert to JSON

@docs toJson

-}

import Json.Encode as E
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompIf
        , end
        , float
        , getChompedString
        , int
        , keyword
        , lazy
        , loop
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set
import String exposing (words)


type Thing
    = Obj (List ( String, Thing ))
    | Dct (List ( Thing, Thing ))
    | Str String
    | Custom String (List Thing)
    | Lst (List Thing)
    | Tpl (List Thing)
    | NumInt Int -- TODO: store as int and float?
    | NumFloat Float -- TODO: store as int and float?
    | Fun


{-| Pretty print output from Debug.toString to JSON
-}
pp : String -> String
pp d =
    let
        v =
            d |> run parse
    in
    case v of
        Ok val ->
            E.encode 4 (encode val)

        Err _ ->
            d



-- JSON


{-| Convert output from Debug.toString to JSON
-}
toJson : String -> Result (List Parser.DeadEnd) E.Value
toJson val =
    val |> run parse |> Result.map encode


encode : Thing -> E.Value
encode thing =
    case thing of
        Obj kvs ->
            kvs |> List.map (\( k, v ) -> ( k, encode v )) |> E.object

        Dct kvs ->
            kvs
                |> List.map
                    (\( k, v ) ->
                        ( case k of
                            Str s ->
                                s

                            _ ->
                                E.encode 0 (encode k)
                        , encode v
                        )
                    )
                |> E.object

        Str s ->
            E.string s

        Custom name args ->
            E.object
                [ ( name
                  , args |> E.list encode
                  )
                ]

        Lst vals ->
            E.list encode vals

        Tpl vals ->
            E.list encode vals

        NumInt n ->
            E.int n

        NumFloat n ->
            E.float n

        Fun ->
            E.string "<function>"



-- PARSER


parse : Parser Thing
parse =
    succeed identity
        |= parseThing
        |. end


parseThing : Parser Thing
parseThing =
    succeed identity
        |= oneOf
            [ parseDct
            , parseObj
            , parseString
            , parseLst
            , parseTpl
            , parseCustom
            , parseNumberFloat
            , parseNumberInt
            , parseFun
            ]
        |. spaces
        |. oneOf [ symbol ",", symbol "" ]
        |. spaces


parseObj : Parser Thing
parseObj =
    succeed Obj
        |. spaces
        |. symbol "{"
        |. spaces
        |= list parseKeyValue
        |. spaces
        |. symbol "}"
        |. spaces


parseDct : Parser Thing
parseDct =
    succeed Dct
        |. spaces
        |. symbol "Dict.fromList ["
        |. spaces
        |= list parseDictKeyValue
        |. spaces
        |. symbol "]"
        |. spaces


parseCustom : Parser Thing
parseCustom =
    succeed Custom
        |. spaces
        |= upperVar
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. spaces


parseLst : Parser Thing
parseLst =
    succeed Lst
        |. spaces
        |. symbol "["
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. symbol "]"
        |. spaces


parseTpl : Parser Thing
parseTpl =
    succeed Tpl
        |. spaces
        |. symbol "("
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. symbol ")"
        |. spaces


parseNumberFloat =
    succeed NumFloat
        |= oneOf
            [ succeed negate
                |. symbol "-"
                |= float
            , float
            ]


parseNumberInt =
    succeed NumInt
        |= oneOf
            [ succeed negate
                |. symbol "-"
                |= int
            , int
            ]


parseFun =
    succeed Fun
        |. symbol "<function>"


upperVar =
    variable { start = Char.isUpper, inner = \c -> Char.isAlphaNum c || c == '.' || c == '_', reserved = Set.empty }


lowerVar =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '.' || c == '_', reserved = Set.empty }


list : Parser a -> Parser (List a)
list parser =
    loop [] (listHelp parser)


listHelp : Parser a -> List a -> Parser (Step (List a) (List a))
listHelp parser acc =
    oneOf
        [ succeed (\val -> Loop (val :: acc))
            |= parser
        , succeed ()
            |> map (\_ -> Done (List.reverse acc))
        ]


parseKeyValue : Parser ( String, Thing )
parseKeyValue =
    succeed Tuple.pair
        |. spaces
        |= lowerVar
        |. spaces
        |. symbol "="
        |. spaces
        |= lazy (\_ -> parseThing)
        |. spaces
        |. oneOf [ symbol ",", symbol "" ]
        |. spaces


parseDictKeyValue : Parser ( Thing, Thing )
parseDictKeyValue =
    succeed Tuple.pair
        |. spaces
        |. symbol "("
        |. spaces
        |= lazy (\_ -> parseThing)
        |. spaces
        |= lazy (\_ -> parseThing)
        |. spaces
        |. symbol ")"
        |. spaces
        |. oneOf [ symbol ",", symbol "" ]
        |. spaces


notQuote : Parser String
notQuote =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> c /= '"')


escapedChar : Parser String
escapedChar =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> c == '\\')
            |. chompIf (\c -> True)


repeat : Parser String -> Parser String
repeat parser =
    loop "" (repeatHelp parser)


repeatHelp : Parser String -> String -> Parser (Step String String)
repeatHelp parser acc =
    oneOf
        [ succeed (\val -> Loop (acc ++ val))
            |= parser
        , succeed ()
            |> map (\_ -> Done acc)
        ]


parseString : Parser Thing
parseString =
    succeed Str
        |. symbol "\""
        |= repeat (oneOf [ escapedChar, notQuote ])
        |. symbol "\""
