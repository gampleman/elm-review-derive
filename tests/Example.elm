module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Review.Test
import Test exposing (..)
import TodoItForMe


suite : Test
suite =
    describe "tests"
        [ test "record codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }

type alias B = { fieldD : Float }

codec : Codec A
codec =
    Codec.record A 
        |> Serialize.field .fieldA Serialize.int 
        |> Serialize.field .fieldB Serialize.string 
        |> Serialize.field .fieldC bCodec 
        |> Serialize.finishRecord

"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }

type alias B = { fieldD : Float }

codec : Codec A
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : Codec A\ncodec = Debug.todo \"\"" }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }

type alias B = { fieldD : Float }

codec : Codec MyType
codec =
    Codec.customType
        (\\a b c value ->
            case value of
                VariantA ->
                    a

                VariantB data0 data1 data2 ->
                    b data0 data1 data2

                VariantC data0 ->
                    c data0
        )
        |> Serialize.variant0 VariantA
        |> Serialize.variant3 VariantB Serialize.int Serialize.string Serialize.float
        |> Serialize.variant1 VariantC myTypeCodec
        |> Serialize.finishCustomType


"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type MyType
    = VariantA
    | VariantB Int String Float
    | VariantC MyType

codec : Codec MyType
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : Codec MyType\ncodec = Debug.todo \"\"" }
                            |> Review.Test.whenFixed expected
                        ]
        ]
