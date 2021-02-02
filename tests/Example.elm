module Example exposing (suite)

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


codec : Codec A.A
codec  =
    Serialize.record A 
        |> Serialize.field .fieldA Serialize.int 
        |> Serialize.field .fieldB Serialize.string 
        |> Serialize.field .fieldC bCodec 
        |> Serialize.finishRecord



bCodec : Codec A.B
bCodec  =
    Serialize.record B 
        |> Serialize.field .fieldD Serialize.float 
        |> Serialize.finishRecord"""
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

type MyType
    = VariantA
    | VariantB Int String Float
    | VariantC MyType


codec : Codec A.MyType
codec  =
    Serialize.customType (\\a b c value -> 
    case value of
      VariantA  ->
        a
      VariantB data0 data1 data2 ->
        b data0 data1 data2
      VariantC data0 ->
        c data0
    ) 
        |> Serialize.variant0 VariantA 
        |> Serialize.variant3 VariantB Serialize.int Serialize.string Serialize.float 
        |> Serialize.variant1 VariantC myTypeCodec 
        |> Serialize.finishCustomType"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type MyType
    = VariantA
    | VariantB Int String Float
    | VariantC MyType

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : Codec MyType\ncodec = Debug.todo \"\"" }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type in another module" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)
import OtherModule


codec : Codec OtherModule.MyType
codec  =
    Serialize.customType (\\a b c value -> 
    case value of
      VariantA  ->
        a
      VariantB data0 data1 data2 ->
        b data0 data1 data2
      VariantC data0 ->
        c data0
    ) 
        |> Serialize.variant0 VariantA 
        |> Serialize.variant3 VariantB Serialize.int Serialize.string Serialize.float 
        |> Serialize.variant1 VariantC myTypeCodec 
        |> Serialize.finishCustomType"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Serialize exposing (Codec)
import OtherModule

codec : Codec OtherModule.MyType
codec = Debug.todo \"\""""
                , """module OtherModule exposing (..)

type MyType
    = VariantA
    | VariantB Int String Float
    | VariantC MyType"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules TodoItForMe.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec OtherModule.MyType\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "maybe codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : Maybe Int }


codec : Codec A.MyType
codec  =
    Serialize.record MyType 
        |> Serialize.field .fieldA (Serialize.maybe Serialize.int) 
        |> Serialize.finishRecord
"""
                in
                "module A exposing (..)\n"
                    ++ "\n"
                    ++ "import Serialize exposing (Codec)\n"
                    ++ "\n"
                    ++ "type alias MyType = { fieldA : Maybe Int }\n"
                    ++ "\n"
                    ++ "codec : Codec MyType\n"
                    ++ "codec = Debug.todo \"\"\n"
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "dict codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType =
    { fieldA : Dict Int String
    }


codec : Codec A.MyType
codec  =
    Serialize.record MyType 
        |> Serialize.field .fieldA (Serialize.dict Serialize.int Serialize.string) 
        |> Serialize.finishRecord

"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec MyType
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "tuple codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : (Float, List String) }


codec : Codec A.MyType
codec  =
    Serialize.record MyType 
        |> Serialize.field .fieldA (Serialize.tuple Serialize.float (Serialize.list Serialize.string)) 
        |> Serialize.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : (Float, List String) }

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "nested record codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }


codec : Codec A.MyType
codec  =
    Serialize.record MyType 
        |> Serialize.field .fieldA (Serialize.record (\\a b -> {field0 = a, field1 = b}) 
        |> Serialize.field .field0 Serialize.int 
        |> Serialize.field .field1 Serialize.unit 
        |> Serialize.finishRecord) 
        |> Serialize.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "add nested codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }


codec : Codec A.MyType
codec  =
    Serialize.record MyType 
        |> Serialize.field .fieldA myOtherTypeCodec 
        |> Serialize.finishRecord

myOtherTypeCodec : Codec A.MyOtherType
myOtherTypeCodec  =
    Serialize.record MyOtherType 
        |> Serialize.field .fieldB Serialize.int 
        |> Serialize.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run TodoItForMe.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "nested codec in another module" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Serialize exposing (Codec)
import B

type alias A = { field : B }

codec : Codec A.A
codec  =
    Serialize.record A
        |> Serialize.field .field codecB
        |> Serialize.finishRecord

codecB : Codec B.B
codecB =
    Serialize.record B
        |> Serialize.field .field Codec.int
        |> Serialize.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Serialize exposing (Codec)
import B

type alias A = { field : B }

codec : Codec A
codec = Debug.todo \"\""""
                , """module B exposing (..)

type alias B = { field : Int }"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules TodoItForMe.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        ]
