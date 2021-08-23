module Tests exposing (suite)

import CodeGen
import Review.Test
import Test exposing (..)


suite : Test
suite =
    describe "tests"
        [ test "record codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : Codec e A
codec =
    Codec.record A
        |> Codec.field .fieldA Codec.int
        |> Codec.field .fieldB Codec.string
        |> Codec.field .fieldC bCodec
        |> Codec.finishRecord


bCodec : Codec e B
bCodec =
    Codec.record B |> Codec.field .fieldD Codec.float |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : Codec e A
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : Codec e A\ncodec = Debug.todo \"\"" }
                            |> Review.Test.whenFixed expected
                        ]
        , test "qualified codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int }

codec : Serialize.Codec e A
codec =
    Codec.record A |> Codec.field .fieldA Codec.int |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int }

codec : Serialize.Codec e A
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Serialize.Codec e A\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : Codec e MyType
codec =
    Codec.customType
     (\\a b c value ->
         case value of
             VariantA ->
                 a

             VariantB data0 ->
                 b data0

             VariantC data0 ->
                 c data0
     )
        |> Codec.variant0 VariantA
        |> Codec.variant1 VariantB Codec.int
        |> Codec.variant1 VariantC myTypeCodec
        |> Codec.finishCustomType"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : Codec e MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : Codec e MyType\ncodec = Debug.todo \"\"" }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type in another module" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)
import OtherModule

codec : Codec e OtherModule.MyType
codec =
    Codec.customType
     (\\a b c value ->
         case value of
             OtherModule.VariantA ->
                 a

             OtherModule.VariantB data0 ->
                 b data0

             OtherModule.VariantC data0 ->
                 c data0
     )
        |> Codec.variant0 OtherModule.VariantA
        |> Codec.variant1 OtherModule.VariantB Codec.int
        |> Codec.variant1 OtherModule.VariantC myTypeCodec
        |> Codec.finishCustomType"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import OtherModule

codec : Codec e OtherModule.MyType
codec = Debug.todo \"\""""
                , """module OtherModule exposing (..)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec e OtherModule.MyType\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "reuse existing codec in another module" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec e A
codec =
    Codec.record A |> Codec.field .field OtherModule.codec |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec e A
codec = Debug.todo \"\""""
                , """module OtherModule exposing (..)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : Codec e MyType
codec  =
    Codec.customType
    (\\a b c value ->
    case value of
      VariantA  ->
        a
      VariantB data0 ->
        b data0
      VariantC data0 ->
        c data0
    )
        |> Codec.variant0 VariantA
        |> Codec.variant1 VariantB Codec.int
        |> Codec.variant1 VariantC myTypeCodec
        |> Codec.finishCustomType"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec e A\ncodec = Debug.todo \"\""
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

import Codec exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec e MyType
codec =
    Codec.record MyType |> Codec.field .fieldA (Codec.maybe Codec.int) |> Codec.finishRecord
"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec e MyType
codec = Debug.todo ""
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "dict codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec e MyType
codec =
    Codec.record MyType |> Codec.field .fieldA (Codec.dict Codec.int Codec.string) |> Codec.finishRecord

"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec e MyType
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "nested record codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec e MyType
codec =
    Codec.record MyType
        |> Codec.field
            .fieldA
            (Codec.record (\\a b -> { field0 = a, field1 = b })
                |> Codec.field .field0 Codec.int
                |> Codec.field .field1 Codec.unit
                |> Codec.finishRecord
            )
        |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec e MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "add nested codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec e MyType
codec =
    Codec.record MyType |> Codec.field .fieldA myOtherTypeCodec |> Codec.finishRecord
myOtherTypeCodec : Codec e MyOtherType
myOtherTypeCodec =
    Codec.record MyOtherType |> Codec.field .fieldB Codec.int |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec e MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e MyType\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "nested codec in another module" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec =
    Codec.record A |> Codec.field .field bCodec |> Codec.finishRecord
bCodec : Codec e B
bCodec =
    Codec.record B |> Codec.field .field Codec.int |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec = Debug.todo \"\""""
                , """module B exposing (..)

type alias B = { field : Int }"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec e A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "twice nested codec in another module" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec =
    Codec.record A |> Codec.field .field bCodec |> Codec.finishRecord
b2Codec : Codec e B.B2
b2Codec =
    Codec.record B.B2 |> Codec.field .field2 Codec.int |> Codec.finishRecord
bCodec : Codec e B
bCodec =
    Codec.record B |> Codec.field .field1 b2Codec |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec = Debug.todo \"\""""
                , """module B exposing (..)

type alias B = { field1 : B2 }

type alias B2 = { field2 : Int }"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec e A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "nested codec in list" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec e A
codec =
    Codec.record A |> Codec.field .field (Codec.list bCodec) |> Codec.finishRecord
bCodec : Codec e B
bCodec =
    Codec.record B |> Codec.field .field1 Codec.int |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec e A
codec = Debug.todo \"\""""
                , """module B exposing (..)

type alias B = { field1 : Int }"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : Codec e A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "codec with type parameter" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec e a -> Codec e (Tree a)
codec codecA =
    Codec.customType
     (\\a b value ->
         case value of
             Node data0 ->
                 a data0

             Leaf data0 ->
                 b data0
     )
        |> Codec.variant1 Node (treeCodec (Debug.todo "Can't handle this"))
        |> Codec.variant1 Leaf (Debug.todo "Can't handle this")
        |> Codec.finishCustomType"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec e a -> Codec e (Tree a)
codec codecA = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e a -> Codec e (Tree a)\ncodec codecA = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "Add codec for type inside tuple" <|
            \_ ->
                let
                    expected =
                        """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec e A
codec =
    Codec.record A |> Codec.field .a (Codec.tuple bCodec Codec.int) |> Codec.finishRecord
bCodec : Codec e B
bCodec =
    Codec.record B |> Codec.field .b Codec.int |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec e A
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e A\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "Add codec for type inside nested record" <|
            \_ ->
                let
                    expected =
                        """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec e A
codec =
    Codec.record A
        |> Codec.field .a (Codec.record (\\a -> { b = a }) |> Codec.field .b bCodec |> Codec.finishRecord)
        |> Codec.finishRecord
bCodec : Codec e B
bCodec =
    Codec.record B |> Codec.field .b Codec.int |> Codec.finishRecord"""
                            |> String.replace "\u{000D}" ""
                in
                """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec e A
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec e A\ncodec = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "tree to string" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree -> String
treeToString tree =
    case tree of
        Node _ _ ->
            "Node"

        Leaf _ ->
            "Leaf\""""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree a -> String
treeToString tree = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "treeToString : Tree a -> String\ntreeToString tree = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "tree to string missing parameter" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree -> String
treeToString value =
    case value of
        Node _ _ ->
            "Node"

        Leaf _ ->
            "Leaf\""""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree a -> String
treeToString = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "treeToString : Tree a -> String\ntreeToString = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type from string" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> Maybe A
fromString a =
    case a of
        "A" ->
            A (Debug.todo "Can't handle this") |> Just

        "B" ->
            B (Debug.todo "Can't handle this") (Debug.todo "Can't handle this") |> Just

        "C" ->
            Just C

        _ ->
            Nothing"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> Maybe (A a)
fromString a = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "fromString : String -> Maybe (A a)\nfromString a = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type from string missing maybe" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> Maybe A
fromString a =
    case a of
        "A" ->
            A (Debug.todo "Can't handle this") |> Just

        "B" ->
            B (Debug.todo "Can't handle this") (Debug.todo "Can't handle this") |> Just

        "C" ->
            Just C

        _ ->
            Nothing"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> A a
fromString a = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "fromString : String -> A a\nfromString a = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "custom type from string missing parameter" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> Maybe A
fromString text =
    case text of
        "A" ->
            A (Debug.todo "Can't handle this") |> Just

        "B" ->
            B (Debug.todo "Can't handle this") (Debug.todo "Can't handle this") |> Just

        "C" ->
            Just C

        _ ->
            Nothing"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> Maybe (A a)
fromString = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "fromString : String -> Maybe (A a)\nfromString = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]
        , test "list all variants" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

list : List A
list =
    [ A (Debug.todo "Can't handle this")
    , B (Debug.todo "Can't handle this") (Debug.todo "Can't handle this")
    , C ]"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type A a
    = A a
    | B Int String
    | C

list : List (A a)
list = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "list : List (A a)\nlist = Debug.todo \"\""
                            }
                            |> Review.Test.whenFixed expected
                        ]

        --        , test "random generator" <|
        --            \_ ->
        --                let
        --                    expected : String
        --                    expected =
        --                        """module A exposing (..)
        --
        --type MyType
        --    = Variant0
        --    | Variant1
        --    | Variant2 NestedType
        --
        --type alias NestedType = { field : (Int, Float) }
        --
        --randomMyType : Random.Generator MyType
        --randomMyType =
        --    Random.uniform
        --        (Random.constant Variant0)
        --        [ Random.constant Variant1, Random.map Variant2 randomNestedType ]
        --        |> Random.andThen identity
        --randomNestedType : Random.Generator NestedType
        --randomNestedType =
        --    Random.constant NestedType |> Random.andMap (Codec.tuple Codec.int Codec.float)
        --"""
        --                            |> String.replace "\u{000D}" ""
        --                in
        --                """module A exposing (..)
        --
        --type MyType
        --    = Variant0
        --    | Variant1
        --    | Variant2 NestedType
        --
        --type alias NestedType = { field : (Int, Float) }
        --
        --randomMyType : Random.Generator MyType
        --randomMyType = Debug.todo \"\""""
        --                    |> String.replace "\u{000D}" ""
        --                    |> Review.Test.run TodoItForMe.rule
        --                    |> Review.Test.expectErrors
        --                        [ Review.Test.error
        --                            { message = "Here's my attempt to complete this stub"
        --                            , details = [ "" ]
        --                            , under = "randomMyType : Random.Generator MyType\nrandomMyType = Debug.todo \"\""
        --                            }
        --                            |> Review.Test.whenFixed expected
        --                        ]
        , test "CaseId.codec not found regression test" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module Route exposing (..)

import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe CaseId)

routeCodec : Codec e Route
routeCodec =
    Codec.customType
     (\\a value ->
         case value of
             MyPagesRoute data0 ->
                 a data0
     )
        |> Codec.variant1 MyPagesRoute (Codec.maybe CaseId.codec)
        |> Codec.finishCustomType
"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module Route exposing (..)

import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe CaseId)

routeCodec : Codec e Route
routeCodec = Debug.todo ""
"""
                , """module CaseId exposing (CaseId, codec)

type CaseId = CaseId String

codec : Codec e CaseId
codec =
    Codec.string |> Codec.map fromString toString
"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Route"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "routeCodec : Codec e Route\nrouteCodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        ]
