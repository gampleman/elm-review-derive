module CsvDecoderCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (FakeDependency, codeGenTest)
import Test exposing (Test, describe)


brianHicksElmCsv3 : FakeDependency
brianHicksElmCsv3 =
    CodeGenerator.Test.fakeDependency
        { name = "BrianHicks/elm-csv"
        , dependencies = []
        , modules =
            [ { name = "Csv.Decode"
              , values =
                    [ ( "andThen", "(a -> Csv.Decode.Decoder b) -> Csv.Decode.Decoder a -> Csv.Decode.Decoder b" )
                    , ( "blank", "Csv.Decode.Decoder a -> Csv.Decode.Decoder (Maybe.Maybe a)" )
                    , ( "column", "Basics.Int -> Csv.Decode.Decoder a -> Csv.Decode.Decoder a" )
                    , ( "decodeCsv", "Csv.Decode.FieldNames -> Csv.Decode.Decoder a -> String.String -> Result.Result Csv.Decode.Error (List.List a)" )
                    , ( "decodeCustom", "{ fieldSeparator : Char.Char } -> Csv.Decode.FieldNames -> Csv.Decode.Decoder a -> String.String -> Result.Result Csv.Decode.Error (List.List a)" )
                    , ( "errorToString", "Csv.Decode.Error -> String.String" )
                    , ( "fail", "String.String -> Csv.Decode.Decoder a" )
                    , ( "field", "String.String -> Csv.Decode.Decoder a -> Csv.Decode.Decoder a" )
                    , ( "float", "Csv.Decode.Decoder Basics.Float" )
                    , ( "fromMaybe", "String.String -> Maybe.Maybe a -> Csv.Decode.Decoder a" )
                    , ( "fromResult", "Result.Result String.String a -> Csv.Decode.Decoder a" )
                    , ( "int", "Csv.Decode.Decoder Basics.Int" )
                    , ( "into", "(a -> b) -> Csv.Decode.Decoder (a -> b)" )
                    , ( "map", "(from -> to) -> Csv.Decode.Decoder from -> Csv.Decode.Decoder to" )
                    , ( "map2", "(a -> b -> c) -> Csv.Decode.Decoder a -> Csv.Decode.Decoder b -> Csv.Decode.Decoder c" )
                    , ( "map3", "(a -> b -> c -> d) -> Csv.Decode.Decoder a -> Csv.Decode.Decoder b -> Csv.Decode.Decoder c -> Csv.Decode.Decoder d" )
                    , ( "oneOf", "Csv.Decode.Decoder a -> List.List (Csv.Decode.Decoder a) -> Csv.Decode.Decoder a" )
                    , ( "pipeline", "Csv.Decode.Decoder a -> Csv.Decode.Decoder (a -> b) -> Csv.Decode.Decoder b" )
                    , ( "string", "Csv.Decode.Decoder String.String" )
                    , ( "succeed", "a -> Csv.Decode.Decoder a" )
                    ]
              }
            , { name = "Csv.Encode"
              , values =
                    [ ( "encode", "{ encoder : Csv.Encode.Encoder a, fieldSeparator : Char.Char } -> List.List a -> String.String" )
                    , ( "withFieldNames", "(a -> List.List ( String.String, String.String )) -> Csv.Encode.Encoder a" )
                    , ( "withoutFieldNames", "(a -> List.List String.String) -> Csv.Encode.Encoder a" )
                    ]
              }
            , { name = "Csv.Parser"
              , values =
                    [ ( "parse", "{ fieldSeparator : Char.Char } -> String.String -> Result.Result Csv.Parser.Problem (List.List (List.List String.String))" )
                    ]
              }
            ]
        }


brianHicksElmCsv4 : FakeDependency
brianHicksElmCsv4 =
    CodeGenerator.Test.fakeDependency
        { name = "BrianHicks/elm-csv"
        , dependencies = []
        , modules =
            [ { name = "Csv.Decode"
              , values =
                    [ ( "andThen", "(a -> Csv.Decode.Decoder b) -> Csv.Decode.Decoder a -> Csv.Decode.Decoder b" )
                    , ( "availableFields", "Csv.Decode.Decoder (List.List String.String)" )
                    , ( "blank", "Csv.Decode.Decoder a -> Csv.Decode.Decoder (Maybe.Maybe a)" )
                    , ( "column", "Basics.Int -> Csv.Decode.Decoder a -> Csv.Decode.Decoder a" )
                    , ( "decodeCsv", "Csv.Decode.FieldNames -> Csv.Decode.Decoder a -> String.String -> Result.Result Csv.Decode.Error (List.List a)" )
                    , ( "decodeCustom", "{ fieldSeparator : Char.Char } -> Csv.Decode.FieldNames -> Csv.Decode.Decoder a -> String.String -> Result.Result Csv.Decode.Error (List.List a)" )
                    , ( "errorToString", "Csv.Decode.Error -> String.String" )
                    , ( "fail", "String.String -> Csv.Decode.Decoder a" )
                    , ( "field", "String.String -> Csv.Decode.Decoder a -> Csv.Decode.Decoder a" )
                    , ( "float", "Csv.Decode.Decoder Basics.Float" )
                    , ( "fromMaybe", "String.String -> Maybe.Maybe a -> Csv.Decode.Decoder a" )
                    , ( "fromResult", "Result.Result String.String a -> Csv.Decode.Decoder a" )
                    , ( "int", "Csv.Decode.Decoder Basics.Int" )
                    , ( "into", "(a -> b) -> Csv.Decode.Decoder (a -> b)" )
                    , ( "map", "(from -> to) -> Csv.Decode.Decoder from -> Csv.Decode.Decoder to" )
                    , ( "map2", "(a -> b -> c) -> Csv.Decode.Decoder a -> Csv.Decode.Decoder b -> Csv.Decode.Decoder c" )
                    , ( "map3", "(a -> b -> c -> d) -> Csv.Decode.Decoder a -> Csv.Decode.Decoder b -> Csv.Decode.Decoder c -> Csv.Decode.Decoder d" )
                    , ( "oneOf", "Csv.Decode.Decoder a -> List.List (Csv.Decode.Decoder a) -> Csv.Decode.Decoder a" )
                    , ( "optionalColumn", "Basics.Int -> Csv.Decode.Decoder a -> Csv.Decode.Decoder (Maybe.Maybe a)" )
                    , ( "optionalField", "String.String -> Csv.Decode.Decoder a -> Csv.Decode.Decoder (Maybe.Maybe a)" )
                    , ( "pipeline", "Csv.Decode.Decoder a -> Csv.Decode.Decoder (a -> b) -> Csv.Decode.Decoder b" )
                    , ( "string", "Csv.Decode.Decoder String.String" )
                    , ( "succeed", "a -> Csv.Decode.Decoder a" )
                    ]
              }
            , { name = "Csv.Encode"
              , values =
                    [ ( "encode", "{ encoder : Csv.Encode.Encoder a, fieldSeparator : Char.Char } -> List.List a -> String.String" )
                    , ( "withFieldNames", "(a -> List.List ( String.String, String.String )) -> Csv.Encode.Encoder a" )
                    , ( "withoutFieldNames", "(a -> List.List String.String) -> Csv.Encode.Encoder a" )
                    ]
              }
            , { name = "Csv.Parser"
              , values =
                    [ ( "parse", "{ fieldSeparator : Char.Char } -> String.String -> Result.Result Csv.Parser.Problem (List.List (List.List String.String))" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "CsvDecoderTodo"
        [ codeGenTest "Basic decoding V3"
            [ brianHicksElmCsv3 ]
            []
            [ """module A exposing (..)

import Csv.Decode exposing (Decoder)

type A b
    = Rec B
    | Gen b

type alias B =
    { float : Float
    , str : String
    , name : Maybe String
    , anon : {a : Int, b : Int}
    }

decode : Decoder (A Int)
decode =
    Debug.todo ""
"""
            ]
            """module A exposing (..)

import Csv.Decode exposing (Decoder)

type A b
    = Rec B
    | Gen b

type alias B =
    { float : Float
    , str : String
    , name : Maybe String
    , anon : {a : Int, b : Int}
    }

decode : Decoder (A Int)
decode =
    Csv.Decode.field "tag" Csv.Decode.string
        |> Csv.Decode.andThen
            (\\ctor ->
                case ctor of
                    "Rec" ->
                        Csv.Decode.map
                            Rec
                            (Csv.Decode.into B
                                |> Csv.Decode.pipeline (Csv.Decode.field "Rec_float" Csv.Decode.float)
                                |> Csv.Decode.pipeline (Csv.Decode.field "Rec_str" Csv.Decode.string)
                                |> Csv.Decode.pipeline
                                    (Csv.Decode.field "Rec_name" (Csv.Decode.blank Csv.Decode.string))
                                |> Csv.Decode.pipeline
                                    (Csv.Decode.map2
                                        (\\a b -> { a = a, b = Csv.Decode.int })
                                        (Csv.Decode.field "Rec_anon_a" Csv.Decode.int)
                                        (Csv.Decode.field "Rec_anon_b" Csv.Decode.int)
                                    )
                            )

                    "Gen" ->
                        Csv.Decode.map Gen (Csv.Decode.field "Gen_0" Csv.Decode.int)

                    _ ->
                        Csv.Decode.fail "Unrecognized constructor"
            )
"""
        , codeGenTest "Basic decoding V4"
            [ brianHicksElmCsv4 ]
            []
            [ """module A exposing (..)

import Csv.Decode exposing (Decoder)

type A b
    = Rec B
    | Gen b

type alias B =
    { float : Float
    , str : String
    , name : Maybe String
    , anon : {a : Int, b : Int}
    }

decode : Decoder (A Int)
decode =
    Debug.todo ""
"""
            ]
            """module A exposing (..)

import Csv.Decode exposing (Decoder)

type A b
    = Rec B
    | Gen b

type alias B =
    { float : Float
    , str : String
    , name : Maybe String
    , anon : {a : Int, b : Int}
    }

decode : Decoder (A Int)
decode =
    Csv.Decode.field "tag" Csv.Decode.string
        |> Csv.Decode.andThen
            (\\ctor ->
                case ctor of
                    "Rec" ->
                        Csv.Decode.map
                            Rec
                            (Csv.Decode.into B
                                |> Csv.Decode.pipeline (Csv.Decode.field "Rec_float" Csv.Decode.float)
                                |> Csv.Decode.pipeline (Csv.Decode.field "Rec_str" Csv.Decode.string)
                                |> Csv.Decode.pipeline
                                    (Csv.Decode.field "Rec_name" (Csv.Decode.blank Csv.Decode.string))
                                |> Csv.Decode.pipeline
                                    (Csv.Decode.map2
                                        (\\a b -> { a = a, b = Csv.Decode.int })
                                        (Csv.Decode.field "Rec_anon_a" Csv.Decode.int)
                                        (Csv.Decode.field "Rec_anon_b" Csv.Decode.int)
                                    )
                            )

                    "Gen" ->
                        Csv.Decode.map Gen (Csv.Decode.field "Gen_0" Csv.Decode.int)

                    _ ->
                        Csv.Decode.fail "Unrecognized constructor"
            )
"""
        ]
