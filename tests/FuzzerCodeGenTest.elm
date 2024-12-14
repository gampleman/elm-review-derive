module FuzzerCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (FakeDependency, codeGenIncrementalTest, codeGenTest)
import StandardModule
import Test exposing (Test, describe)


elmJson : FakeDependency
elmJson =
    CodeGenerator.Test.fakeDependency
        { name = "elm-explorations/test"
        , dependencies = []
        , modules =
            [ { name = "Fuzz"
              , values =
                    [ ( "andMap", "Fuzz.Fuzzer a -> Fuzz.Fuzzer (a -> b) -> Fuzz.Fuzzer b" )
                    , ( "andThen", "(a -> Fuzz.Fuzzer b) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b" )
                    , ( "array", "Fuzz.Fuzzer a -> Fuzz.Fuzzer (Array.Array a)" )
                    , ( "asciiChar", "Fuzz.Fuzzer Char.Char" )
                    , ( "asciiString", "Fuzz.Fuzzer String.String" )
                    , ( "asciiStringOfLength", "Basics.Int -> Fuzz.Fuzzer String.String" )
                    , ( "asciiStringOfLengthBetween", "Basics.Int -> Basics.Int -> Fuzz.Fuzzer String.String" )
                    , ( "bool", "Fuzz.Fuzzer Basics.Bool" )
                    , ( "char", "Fuzz.Fuzzer Char.Char" )
                    , ( "constant", "a -> Fuzz.Fuzzer a" )
                    , ( "examples", "Basics.Int -> Fuzz.Fuzzer a -> List.List a" )
                    , ( "filter", "(a -> Basics.Bool) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer a" )
                    , ( "float", "Fuzz.Fuzzer Basics.Float" )
                    , ( "floatAtLeast", "Basics.Float -> Fuzz.Fuzzer Basics.Float" )
                    , ( "floatAtMost", "Basics.Float -> Fuzz.Fuzzer Basics.Float" )
                    , ( "floatRange", "Basics.Float -> Basics.Float -> Fuzz.Fuzzer Basics.Float" )
                    , ( "frequency", "List.List ( Basics.Float, Fuzz.Fuzzer a ) -> Fuzz.Fuzzer a" )
                    , ( "frequencyValues", "List.List ( Basics.Float, a ) -> Fuzz.Fuzzer a" )
                    , ( "fromGenerator", "Random.Generator a -> Fuzz.Fuzzer a" )
                    , ( "int", "Fuzz.Fuzzer Basics.Int" )
                    , ( "intAtLeast", "Basics.Int -> Fuzz.Fuzzer Basics.Int" )
                    , ( "intAtMost", "Basics.Int -> Fuzz.Fuzzer Basics.Int" )
                    , ( "intRange", "Basics.Int -> Basics.Int -> Fuzz.Fuzzer Basics.Int" )
                    , ( "invalid", "String.String -> Fuzz.Fuzzer a" )
                    , ( "labelExamples", "Basics.Int -> List.List ( String.String, a -> Basics.Bool ) -> Fuzz.Fuzzer a -> List.List ( List.List String.String, Maybe.Maybe a )" )
                    , ( "lazy", "(() -> Fuzz.Fuzzer a) -> Fuzz.Fuzzer a" )
                    , ( "list", "Fuzz.Fuzzer a -> Fuzz.Fuzzer (List.List a)" )
                    , ( "listOfLength", "Basics.Int -> Fuzz.Fuzzer a -> Fuzz.Fuzzer (List.List a)" )
                    , ( "listOfLengthBetween", "Basics.Int -> Basics.Int -> Fuzz.Fuzzer a -> Fuzz.Fuzzer (List.List a)" )
                    , ( "map", "(a -> b) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b" )
                    , ( "map2", "(a -> b -> c) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c" )
                    , ( "map3", "(a -> b -> c -> d) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d" )
                    , ( "map4", "(a -> b -> c -> d -> e) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d -> Fuzz.Fuzzer e" )
                    , ( "map5", "(a -> b -> c -> d -> e -> f) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d -> Fuzz.Fuzzer e -> Fuzz.Fuzzer f" )
                    , ( "map6", "(a -> b -> c -> d -> e -> f -> g) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d -> Fuzz.Fuzzer e -> Fuzz.Fuzzer f -> Fuzz.Fuzzer g" )
                    , ( "map7", "(a -> b -> c -> d -> e -> f -> g -> h) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d -> Fuzz.Fuzzer e -> Fuzz.Fuzzer f -> Fuzz.Fuzzer g -> Fuzz.Fuzzer h" )
                    , ( "map8", "(a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d -> Fuzz.Fuzzer e -> Fuzz.Fuzzer f -> Fuzz.Fuzzer g -> Fuzz.Fuzzer h -> Fuzz.Fuzzer i" )
                    , ( "maybe", "Fuzz.Fuzzer a -> Fuzz.Fuzzer (Maybe.Maybe a)" )
                    , ( "niceFloat", "Fuzz.Fuzzer Basics.Float" )
                    , ( "oneOf", "List.List (Fuzz.Fuzzer a) -> Fuzz.Fuzzer a" )
                    , ( "oneOfValues", "List.List a -> Fuzz.Fuzzer a" )
                    , ( "order", "Fuzz.Fuzzer Basics.Order" )
                    , ( "pair", "Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer ( a, b )" )
                    , ( "percentage", "Fuzz.Fuzzer Basics.Float" )
                    , ( "result", "Fuzz.Fuzzer error -> Fuzz.Fuzzer value -> Fuzz.Fuzzer (Result.Result error value)" )
                    , ( "sequence", "List.List (Fuzz.Fuzzer a) -> Fuzz.Fuzzer (List.List a)" )
                    , ( "shuffledList", "List.List a -> Fuzz.Fuzzer (List.List a)" )
                    , ( "string", "Fuzz.Fuzzer String.String" )
                    , ( "stringOfLength", "Basics.Int -> Fuzz.Fuzzer String.String" )
                    , ( "stringOfLengthBetween", "Basics.Int -> Basics.Int -> Fuzz.Fuzzer String.String" )
                    , ( "traverse", "(a -> Fuzz.Fuzzer b) -> List.List a -> Fuzz.Fuzzer (List.List b)" )
                    , ( "triple", "Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer ( a, b, c )" )
                    , ( "uniformInt", "Basics.Int -> Fuzz.Fuzzer Basics.Int" )
                    , ( "unit", "Fuzz.Fuzzer ()" )
                    , ( "weightedBool", "Basics.Float -> Fuzz.Fuzzer Basics.Bool" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "testFuzzerTodo"
        [ codeGenTest "Standard"
            [ elmJson ]
            []
            [ StandardModule.standard
            , """module A exposing (..)
import Standard exposing (..)
import Fuzz exposing (Fuzzer)

fuzzer : Fuzzer (A Int)
fuzzer =
    Debug.todo ""
"""
            ]
            """module A exposing (..)
import Standard exposing (..)
import Fuzz exposing (Fuzzer)

import Dict

fuzzer : Fuzzer (A Int)
fuzzer =
    Fuzz.oneOf [ Fuzz.map Rec bFuzzer, Fuzz.map Gen Fuzz.int, Fuzz.map Recursive (Fuzz.lazy (\\() -> fuzzer)) ]

bFuzzer : Fuzzer B
bFuzzer =
    Fuzz.map5
        B
        (Fuzz.list Fuzz.int)
        (Fuzz.array Fuzz.string)
        (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair Fuzz.string Fuzz.niceFloat)))
        (Fuzz.pair (Fuzz.maybe Fuzz.string) Fuzz.bool)
        (Fuzz.map2 (\\a b -> { a = a, b = b }) Fuzz.int Fuzz.int)
"""
        , codeGenIncrementalTest "Issue #16"
            [ elmJson ]
            []
            [ """module SQLite.Statement.CreateTable exposing (ColumnConstraint(..), InnerColumnConstraint(..))

type alias ColumnConstraint =
    { foo : (Maybe String), bar : InnerColumnConstraint }

type InnerColumnConstraint =
    InnerColumnConstraint String
"""
            , """module ParserTest exposing (suite)

import SQLite.Statement.CreateTable as CreateTable
import Fuzz exposing (Fuzzer)

columnConstraintFuzzer : Fuzzer CreateTable.ColumnConstraint
columnConstraintFuzzer =
    Debug.todo ""
"""
            ]
            """module ParserTest exposing (suite)

import SQLite.Statement.CreateTable as CreateTable
import Fuzz exposing (Fuzzer)

columnConstraintFuzzer : Fuzzer CreateTable.ColumnConstraint
columnConstraintFuzzer =
    Fuzz.map2 CreateTable.ColumnConstraint (Fuzz.maybe Fuzz.string) innerColumnConstraintFuzzer

innerColumnConstraintFuzzer : Fuzzer CreateTable.InnerColumnConstraint
innerColumnConstraintFuzzer =
    Debug.todo ""
"""
        , codeGenTest "Issue #17"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Fuzz exposing (Fuzzer)

type Semaphore = Red | Yellow | Green

fuzzer : Fuzzer Semaphore
fuzzer =
    Debug.todo ""
"""
            ]
            """module A exposing (..)
import Fuzz exposing (Fuzzer)

type Semaphore = Red | Yellow | Green

fuzzer : Fuzzer Semaphore
fuzzer =
    Fuzz.oneOfValues [ Red, Yellow, Green ]
"""
        ]
