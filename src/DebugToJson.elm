module DebugToJson exposing (..)

import Json.Encode as E
import Json.Print
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


debugToJsonString : String -> String
debugToJsonString val =
    val
        |> ppp



--|> (++) (val ++ "\n\n")


type Thing
    = Obj (List ( String, Thing ))
    | Str String
    | Custom String (List Thing)
    | Lst (List Thing)
    | Tpl (List Thing)
    | Num String -- TODO: store as int and float?
    | Fun



-- JSON


encode : Thing -> E.Value
encode thing =
    case thing of
        Obj kvs ->
            kvs |> List.map (\( k, v ) -> ( k, encode v )) |> E.object

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

        Num s ->
            E.string s

        Fun ->
            E.string "<function>"


ppp d =
    let
        v =
            d |> run parseDebug
    in
    case v of
        Ok val ->
            case Json.Print.prettyValue { indent = 4, columns = 120 } (encode val) of
                Ok s ->
                    s

                Err s ->
                    s

        Err _ ->
            "ERR"


pp val =
    case Json.Print.prettyString { indent = 2, columns = 120 } val of
        Ok s ->
            s

        Err s ->
            s



-- PARSER


parseDebug : Parser Thing
parseDebug =
    succeed identity
        |= parseThing
        |. end


parseThing : Parser Thing
parseThing =
    succeed identity
        |= oneOf
            [ parseObj
            , parseString
            , parseLst
            , parseTpl
            , parseCustom
            , parseNumber
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


parseNumber =
    succeed Num
        |= variable
            { start = \c -> Char.isDigit c || c == '-'
            , inner = \c -> Char.isHexDigit c || c == 'x' || c == '.'
            , reserved = Set.empty
            }


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
