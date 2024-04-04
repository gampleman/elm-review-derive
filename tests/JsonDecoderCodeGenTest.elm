module JsonDecoderCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (FakeDependency, codeGenTest)
import StandardModule
import Test exposing (Test, describe)


elmJson : FakeDependency
elmJson =
    CodeGenerator.Test.fakeDependency
        { name = "elm/json"
        , dependencies = []
        , modules =
            [ { name = "Json.Decode"
              , values =
                    [ ( "andThen", "(a -> Json.Decode.Decoder b) -> Json.Decode.Decoder a -> Json.Decode.Decoder b" )
                    , ( "array", "Json.Decode.Decoder a -> Json.Decode.Decoder (Array.Array a)" )
                    , ( "at", "List.List String.String -> Json.Decode.Decoder a -> Json.Decode.Decoder a" )
                    , ( "bool", "Json.Decode.Decoder Basics.Bool" )
                    , ( "decodeString", "Json.Decode.Decoder a -> String.String -> Result.Result Json.Decode.Error a" )
                    , ( "decodeValue", "Json.Decode.Decoder a -> Json.Decode.Value -> Result.Result Json.Decode.Error a" )
                    , ( "dict", "Json.Decode.Decoder a -> Json.Decode.Decoder (Dict.Dict String.String a)" )
                    , ( "errorToString", "Json.Decode.Error -> String.String" )
                    , ( "fail", "String.String -> Json.Decode.Decoder a" )
                    , ( "field", "String.String -> Json.Decode.Decoder a -> Json.Decode.Decoder a" )
                    , ( "float", "Json.Decode.Decoder Basics.Float" )
                    , ( "index", "Basics.Int -> Json.Decode.Decoder a -> Json.Decode.Decoder a" )
                    , ( "int", "Json.Decode.Decoder Basics.Int" )
                    , ( "keyValuePairs", "Json.Decode.Decoder a -> Json.Decode.Decoder (List.List ( String.String, a ))" )
                    , ( "lazy", "(() -> Json.Decode.Decoder a) -> Json.Decode.Decoder a" )
                    , ( "list", "Json.Decode.Decoder a -> Json.Decode.Decoder (List.List a)" )
                    , ( "map", "(a -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder value" )
                    , ( "map2", "(a -> b -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder value" )
                    , ( "map3", "(a -> b -> c -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder value" )
                    , ( "map4", "(a -> b -> c -> d -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder d -> Json.Decode.Decoder value" )
                    , ( "map5", "(a -> b -> c -> d -> e -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder d -> Json.Decode.Decoder e -> Json.Decode.Decoder value" )
                    , ( "map6", "(a -> b -> c -> d -> e -> f -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder d -> Json.Decode.Decoder e -> Json.Decode.Decoder f -> Json.Decode.Decoder value" )
                    , ( "map7", "(a -> b -> c -> d -> e -> f -> g -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder d -> Json.Decode.Decoder e -> Json.Decode.Decoder f -> Json.Decode.Decoder g -> Json.Decode.Decoder value" )
                    , ( "map8", "(a -> b -> c -> d -> e -> f -> g -> h -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder c -> Json.Decode.Decoder d -> Json.Decode.Decoder e -> Json.Decode.Decoder f -> Json.Decode.Decoder g -> Json.Decode.Decoder h -> Json.Decode.Decoder value" )
                    , ( "maybe", "Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe.Maybe a)" )
                    , ( "null", "a -> Json.Decode.Decoder a" )
                    , ( "nullable", "Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe.Maybe a)" )
                    , ( "oneOf", "List.List (Json.Decode.Decoder a) -> Json.Decode.Decoder a" )
                    , ( "oneOrMore", "(a -> List.List a -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder value" )
                    , ( "string", "Json.Decode.Decoder String.String" )
                    , ( "succeed", "a -> Json.Decode.Decoder a" )
                    , ( "value", "Json.Decode.Decoder Json.Decode.Value" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "JsonDecoderTodo"
        [ codeGenTest "Standard"
            [ elmJson ]
            []
            [ StandardModule.standard
            , """module A exposing (..)
import Standard exposing (..)
import Json.Decode exposing (Decoder)

decode : Decoder (A Int)
decode =
    Debug.todo ""
"""
            ]
            """module A exposing (..)
import Standard exposing (..)
import Json.Decode exposing (Decoder)

decode : Decoder (A Int)
decode =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\\ctor ->
                case ctor of
                    "Rec" ->
                        Json.Decode.map Rec (Json.Decode.field "0" decodeB)

                    "Gen" ->
                        Json.Decode.map Gen (Json.Decode.field "0" Json.Decode.int)

                    "Recursive" ->
                        Json.Decode.map Recursive (Json.Decode.field "0" decode)

                    _ ->
                        Json.Decode.fail "Unrecognized constructor"
            )

decodeB : Decoder B
decodeB =
    Json.Decode.map5
        B
        (Json.Decode.field "list" (Json.Decode.list Json.Decode.int))
        (Json.Decode.field "array" (Json.Decode.array Json.Decode.string))
        (Json.Decode.field "dict" (Json.Decode.dict Json.Decode.float))
        (Json.Decode.field
            "tuple"
            (Json.Decode.map2
                Tuple.pair
                (Json.Decode.index 0 (Json.Decode.maybe Json.Decode.string))
                (Json.Decode.index 1 Json.Decode.bool)
            )
        )
        (Json.Decode.field
            "anon"
            (Json.Decode.map2
                (\\a b -> { a = a, b = b })
                (Json.Decode.field "a" Json.Decode.int)
                (Json.Decode.field "b" Json.Decode.int)
            )
        )
"""
        ]
