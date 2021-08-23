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

codec : Codec A
codec =
    Codec.object A
        |> Codec.field "fieldA" .fieldA Codec.int
        |> Codec.field "fieldB" .fieldB Codec.string
        |> Codec.field "fieldC" .fieldC bCodec
        |> Codec.buildObject


bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "fieldD" .fieldD Codec.float |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : Codec A
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : Codec A\ncodec = Debug.todo \"\"" }
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

codec : Codec.Codec A
codec =
    Codec.object A |> Codec.field "fieldA" .fieldA Codec.int |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int }

codec : Codec.Codec A
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec.Codec A\ncodec = Debug.todo \"\""
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

import Json.Decode

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : Codec MyType
codec =
    Codec.custom
     "tag"
     (\\a b c value ->
         case value of
             VariantA ->
                 a

             VariantB data0 ->
                 b data0

             VariantC data0 ->
                 c data0
     )
        |> Codec.variant0 "VariantA" VariantA
        |> Codec.variant1Data "VariantB" VariantB Codec.int
        |> Codec.variant1Data "VariantC" VariantC myTypeCodec
        |> Codec.buildCustom (Json.Decode.fail << (++) "Unknown MyType tag: ")"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
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

import Codec exposing (Codec)
import OtherModule

import Json.Decode

codec : Codec OtherModule.MyType
codec =
    Codec.custom
     "tag"
     (\\a b c value ->
         case value of
             OtherModule.VariantA ->
                 a

             OtherModule.VariantB data0 ->
                 b data0

             OtherModule.VariantC data0 ->
                 c data0
     )
        |> Codec.variant0 "VariantA" OtherModule.VariantA
        |> Codec.variant1Data "VariantB" OtherModule.VariantB Codec.int
        |> Codec.variant1Data "VariantC" OtherModule.VariantC myTypeCodec
        |> Codec.buildCustom (Json.Decode.fail << (++) "Unknown MyType tag: ")"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import OtherModule

codec : Codec OtherModule.MyType
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
                                , under = "codec : Codec OtherModule.MyType\ncodec = Debug.todo \"\""
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

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field OtherModule.codec |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec A
codec = Debug.todo \"\""""
                , """module OtherModule exposing (..)

import Json.Decode

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : Codec MyType
codec  =
    Codec.custom
    "tag"
    (\\a b c value ->
    case value of
      VariantA  ->
        a
      VariantB data0 ->
        b data0
      VariantC data0 ->
        c data0
    )
        |> Codec.variant0 "VariantA" VariantA
        |> Codec.variant1Data "VariantB" VariantB Codec.int
        |> Codec.variant1Data "VariantC" VariantC myTypeCodec
        |> Codec.buildCustom (Json.Decode.fail << (++) "Unknown MyType tag: ")"""
                ]
                    |> List.map (String.replace "\u{000D}" "")
                    |> Review.Test.runOnModules CodeGen.rule
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
        , test "maybe codec" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec MyType
codec =
    Codec.object MyType |> Codec.field "fieldA" .fieldA (Codec.nullable Codec.int) |> Codec.buildObject
"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec MyType
codec = Debug.todo ""
"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
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

import Codec exposing (Codec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec MyType
codec =
    Codec.object MyType |> Codec.field "fieldA" .fieldA (Codec.dict Codec.int Codec.string) |> Codec.buildObject

"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec MyType
codec = Debug.todo ""

"""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
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

import Codec exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec MyType
codec =
    Codec.object MyType
        |> Codec.field
            "fieldA"
            .fieldA
            (Codec.object (\\a b -> { field0 = a, field1 = b })
                |> Codec.field "field0" .field0 Codec.int
                |> Codec.field "field1" .field1 Codec.unit
                |> Codec.buildObject
            )
        |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
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

import Codec exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec MyType
codec =
    Codec.object MyType |> Codec.field "fieldA" .fieldA myOtherTypeCodec |> Codec.buildObject
myOtherTypeCodec : Codec MyOtherType
myOtherTypeCodec =
    Codec.object MyOtherType |> Codec.field "fieldB" .fieldB Codec.int |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec MyType
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
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

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field bCodec |> Codec.buildObject
bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "field" .field Codec.int |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
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
                                , under = "codec : Codec A\ncodec = Debug.todo \"\""
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

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field bCodec |> Codec.buildObject
b2Codec : Codec B.B2
b2Codec =
    Codec.object B.B2 |> Codec.field "field2" .field2 Codec.int |> Codec.buildObject
bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "field1" .field1 b2Codec |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
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
                                , under = "codec : Codec A\ncodec = Debug.todo \"\""
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

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field (Codec.list bCodec) |> Codec.buildObject
bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "field1" .field1 Codec.int |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec A
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
                                , under = "codec : Codec A\ncodec = Debug.todo \"\""
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

import Json.Decode

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec a -> Codec (Tree a)
codec codecA =
    Codec.custom
     "tag"
     (\\a b value ->
         case value of
             Node data0 ->
                 a data0

             Leaf data0 ->
                 b data0
     )
        |> Codec.variant1Data "Node" Node (treeCodec (Debug.todo "Can't handle this"))
        |> Codec.variant1Data "Leaf" Leaf (Debug.todo "Can't handle this")
        |> Codec.buildCustom (Json.Decode.fail << (++) "Unknown Tree tag: ")"""
                            |> String.replace "\u{000D}" ""
                in
                """module A exposing (..)

import Codec exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec a -> Codec (Tree a)
codec codecA = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec a -> Codec (Tree a)\ncodec codecA = Debug.todo \"\""
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

codec : Codec A
codec =
    Codec.object A |> Codec.field "a" .a (Codec.tuple bCodec Codec.int) |> Codec.buildObject
bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "b" .b Codec.int |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec A
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec A\ncodec = Debug.todo \"\""
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

codec : Codec A
codec =
    Codec.object A
        |> Codec.field "a" .a (Codec.object (\\a -> { b = a }) |> Codec.field "b" .b bCodec |> Codec.buildObject)
        |> Codec.buildObject
bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "b" .b Codec.int |> Codec.buildObject"""
                            |> String.replace "\u{000D}" ""
                in
                """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec A
codec = Debug.todo \"\""""
                    |> String.replace "\u{000D}" ""
                    |> Review.Test.run CodeGen.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = "codec : Codec A\ncodec = Debug.todo \"\""
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

import Json.Decode

type Route
    = MyPagesRoute (Maybe CaseId)

routeCodec : Codec Route
routeCodec =
    Codec.custom
     "tag"
     (\\a value ->
         case value of
             MyPagesRoute data0 ->
                 a data0
     )
        |> Codec.variant1Data "MyPagesRoute" MyPagesRoute (Codec.nullable CaseId.codec)
        |> Codec.buildCustom (Json.Decode.fail << (++) "Unknown Route tag: ")
"""
                            |> String.replace "\u{000D}" ""
                in
                [ """module Route exposing (..)

import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe CaseId)

routeCodec : Codec Route
routeCodec = Debug.todo ""
"""
                , """module CaseId exposing (CaseId, codec)

type CaseId = CaseId String

codec : Codec CaseId
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
                                , under = "routeCodec : Codec Route\nrouteCodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , describe "AutoCodec"
            [ test "record AutoCodec" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : AutoCodec A
codec =
    AutoCodec.object A
        |> AutoCodec.field "fieldA" .fieldA AutoCodec.int
        |> AutoCodec.field "fieldB" .fieldB AutoCodec.string
        |> AutoCodec.field "fieldC" .fieldC bCodec
        |> AutoCodec.buildObject


bCodec : AutoCodec B
bCodec =
    AutoCodec.object B |> AutoCodec.field "fieldD" .fieldD AutoCodec.float |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : AutoCodec A
codec = Debug.todo ""

"""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : AutoCodec A\ncodec = Debug.todo \"\"" }
                                |> Review.Test.whenFixed expected
                            ]
            , test "qualified AutoCodec" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A = { fieldA : Int }

codec : AutoCodec.AutoCodec A
codec =
    AutoCodec.object A |> AutoCodec.field "fieldA" .fieldA AutoCodec.int |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A = { fieldA : Int }

codec : AutoCodec.AutoCodec A
codec = Debug.todo \"\""""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec.AutoCodec A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            , test "custom type AutoCodec" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : AutoCodec MyType
codec =
    AutoCodec.custom
     (\\a b c value ->
         case value of
             VariantA ->
                 a

             VariantB data0 ->
                 b data0

             VariantC data0 ->
                 c data0
     )
        |> AutoCodec.variant0 "VariantA" VariantA
        |> AutoCodec.variant1 "VariantB" VariantB AutoCodec.int
        |> AutoCodec.variant1 "VariantC" VariantC myTypeCodec
        |> AutoCodec.buildCustom"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : AutoCodec MyType
codec = Debug.todo \"\""""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = "codec : AutoCodec MyType\ncodec = Debug.todo \"\"" }
                                |> Review.Test.whenFixed expected
                            ]
            , test "custom type in another module" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)
import OtherModule

codec : AutoCodec OtherModule.MyType
codec =
    AutoCodec.custom
     (\\a b c value ->
         case value of
             OtherModule.VariantA ->
                 a

             OtherModule.VariantB data0 ->
                 b data0

             OtherModule.VariantC data0 ->
                 c data0
     )
        |> AutoCodec.variant0 "VariantA" OtherModule.VariantA
        |> AutoCodec.variant1 "VariantB" OtherModule.VariantB AutoCodec.int
        |> AutoCodec.variant1 "VariantC" OtherModule.VariantC myTypeCodec
        |> AutoCodec.buildCustom"""
                                |> String.replace "\u{000D}" ""
                    in
                    [ """module A exposing (..)

import AutoCodec exposing (AutoCodec)
import OtherModule

codec : AutoCodec OtherModule.MyType
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
                                    , under = "codec : AutoCodec OtherModule.MyType\ncodec = Debug.todo \"\""
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

import AutoCodec exposing (AutoCodec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : AutoCodec A
codec =
    AutoCodec.object A |> AutoCodec.field "field" .field OtherModule.codec |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    [ """module A exposing (..)

import AutoCodec exposing (AutoCodec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : AutoCodec A
codec = Debug.todo \"\""""
                    , """module OtherModule exposing (..)

import Json.Decode

type MyType
    = VariantA
    | VariantB Int
    | VariantC MyType

codec : AutoCodec MyType
codec  =
    AutoCodec.custom
    (\\a b c value ->
    case value of
      VariantA  ->
        a
      VariantB data0 ->
        b data0
      VariantC data0 ->
        c data0
    )
        |> AutoCodec.variant0 "VariantA" VariantA
        |> AutoCodec.variant1 "VariantB" VariantB AutoCodec.int
        |> AutoCodec.variant1 "VariantC" VariantC myTypeCodec
        |> AutoCodec.buildCustom"""
                    ]
                        |> List.map (String.replace "\u{000D}" "")
                        |> Review.Test.runOnModules CodeGen.rule
                        |> Review.Test.expectErrorsForModules
                            [ ( "A"
                              , [ Review.Test.error
                                    { message = "Here's my attempt to complete this stub"
                                    , details = [ "" ]
                                    , under = "codec : AutoCodec A\ncodec = Debug.todo \"\""
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

import AutoCodec exposing (AutoCodec)

type alias MyType = { fieldA : Maybe Int }

codec : AutoCodec MyType
codec =
    AutoCodec.object MyType
        |> AutoCodec.field "fieldA" .fieldA (AutoCodec.nullable AutoCodec.int)
        |> AutoCodec.buildObject
"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType = { fieldA : Maybe Int }

codec : AutoCodec MyType
codec = Debug.todo ""
"""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec MyType\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            , test "dict codec" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : AutoCodec MyType
codec =
    AutoCodec.object MyType
        |> AutoCodec.field "fieldA" .fieldA (AutoCodec.dict AutoCodec.int AutoCodec.string)
        |> AutoCodec.buildObject

"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : AutoCodec MyType
codec = Debug.todo ""

"""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec MyType\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            , test "nested record codec" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : AutoCodec MyType
codec =
    AutoCodec.object MyType
        |> AutoCodec.field
            "fieldA"
            .fieldA
            (AutoCodec.object (\\a b -> { field0 = a, field1 = b })
                |> AutoCodec.field "field0" .field0 AutoCodec.int
                |> AutoCodec.field "field1" .field1 AutoCodec.unit
                |> AutoCodec.buildObject
            )
        |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : AutoCodec MyType
codec = Debug.todo \"\""""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec MyType\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            , test "add nested codec" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : AutoCodec MyType
codec =
    AutoCodec.object MyType |> AutoCodec.field "fieldA" .fieldA myOtherTypeCodec |> AutoCodec.buildObject
myOtherTypeCodec : AutoCodec MyOtherType
myOtherTypeCodec =
    AutoCodec.object MyOtherType |> AutoCodec.field "fieldB" .fieldB AutoCodec.int |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module A exposing (..)

import AutoCodec exposing (AutoCodec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : AutoCodec MyType
codec = Debug.todo \"\""""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec MyType\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            , test "Add codec for type inside tuple" <|
                \_ ->
                    let
                        expected =
                            """module Schema exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : AutoCodec A
codec =
    AutoCodec.object A |> AutoCodec.field "a" .a (AutoCodec.tuple bCodec AutoCodec.int) |> AutoCodec.buildObject
bCodec : AutoCodec B
bCodec =
    AutoCodec.object B |> AutoCodec.field "b" .b AutoCodec.int |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module Schema exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : AutoCodec A
codec = Debug.todo \"\""""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            , test "Add codec for type inside nested record" <|
                \_ ->
                    let
                        expected =
                            """module Schema exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : AutoCodec A
codec =
    AutoCodec.object A
        |> AutoCodec.field
            "a"
            .a
            (AutoCodec.object (\\a -> { b = a }) |> AutoCodec.field "b" .b bCodec |> AutoCodec.buildObject)
        |> AutoCodec.buildObject
bCodec : AutoCodec B
bCodec =
    AutoCodec.object B |> AutoCodec.field "b" .b AutoCodec.int |> AutoCodec.buildObject"""
                                |> String.replace "\u{000D}" ""
                    in
                    """module Schema exposing (..)

import AutoCodec exposing (AutoCodec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : AutoCodec A
codec = Debug.todo \"\""""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run CodeGen.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "codec : AutoCodec A\ncodec = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
            ]
        ]
