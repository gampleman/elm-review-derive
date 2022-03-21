module SerializeCodeGenTest exposing (suite)

import NoDebug.Todo
import Review.Project.Dependency exposing (Dependency)
import Review.Test
import Test exposing (..)
import TestHelper exposing (codeGenTest)


elmSerialize : Dependency
elmSerialize =
    TestHelper.fakeDependency "MartinSStewart/elm-serialize"


suite : Test
suite =
    describe "Serialize code gen todo"
        [ codeGenTest "record codec" [ elmSerialize ] [ """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : Codec e A
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : Codec e A
codec =
    Serialize.record A
        |> Serialize.field .fieldA Serialize.int
        |> Serialize.field .fieldB Serialize.string
        |> Serialize.field .fieldC bCodec
        |> Serialize.finishRecord

bCodec : Codec e B
bCodec =
    Serialize.record B |> Serialize.field .fieldD Serialize.float |> Serialize.finishRecord
"""
        , codeGenTest "qualified codec" [ elmSerialize ] [ """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int }

codec : Serialize.Codec e A
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type alias A = { fieldA : Int }

codec : Serialize.Codec e A
codec =
    Serialize.record A |> Serialize.field .fieldA Serialize.int |> Serialize.finishRecord
"""
        , codeGenTest "custom type codec"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String String

codec : Codec e MyType
codec =
    Debug.todo ""
""" ]
            """module A exposing (..)

import Serialize exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String String

codec : Codec e MyType
codec =
    Serialize.customType
        (\\variantAEncoder variantBEncoder variantCEncoder value ->
            case value of
                VariantA ->
                    variantAEncoder

                VariantB arg0 ->
                    variantBEncoder arg0

                VariantC arg0 arg1 ->
                    variantCEncoder arg0 arg1
        )
        |> Serialize.variant0 VariantA
        |> Serialize.variant1 VariantB Serialize.int
        |> Serialize.variant2 VariantC Serialize.string Serialize.string
        |> Serialize.finishCustomType
"""
        , codeGenTest "custom type in another module"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)
import OtherModule

codec : Codec e OtherModule.MyType
codec =
    Debug.todo ""
"""
            , """module OtherModule exposing (..)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String
"""
            ]
            """module A exposing (..)

import Serialize exposing (Codec)
import OtherModule

codec : Codec e OtherModule.MyType
codec =
    Serialize.customType
        (\\variantAEncoder variantBEncoder variantCEncoder value ->
            case value of
                OtherModule.VariantA ->
                    variantAEncoder

                OtherModule.VariantB arg0 ->
                    variantBEncoder arg0

                OtherModule.VariantC arg0 ->
                    variantCEncoder arg0
        )
        |> Serialize.variant0 OtherModule.VariantA
        |> Serialize.variant1 OtherModule.VariantB Serialize.int
        |> Serialize.variant1 OtherModule.VariantC Serialize.string
        |> Serialize.finishCustomType
"""
        , codeGenTest "reuse existing codec in another module"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec e A
codec =
    Debug.todo ""
"""
            , """module OtherModule exposing (..)

import Serialize exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String

codec : Codec e MyType
codec  =
    Serialize.customType
    (\\a b c value ->
    case value of
      VariantA  ->
        a
      VariantB data0 ->
        b data0
      VariantC data0 ->
        c data0
    )
        |> Serialize.variant0 VariantA
        |> Serialize.variant1 VariantB Serialize.int
        |> Serialize.variant1 VariantC Serialize.string
        |> Serialize.finishCustomType"""
            ]
            """module A exposing (..)

import Serialize exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec e A
codec =
    Serialize.record A |> Serialize.field .field OtherModule.codec |> Serialize.finishRecord
"""
        , codeGenTest "maybe codec" [ elmSerialize ] [ """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec e MyType
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec e MyType
codec =
    Serialize.record MyType |> Serialize.field .fieldA (Serialize.maybe Serialize.int) |> Serialize.finishRecord
"""
        , codeGenTest "dict codec" [ elmSerialize ] [ """module A exposing (..)

import Serialize exposing (Codec)
import Dict exposing (Dict)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec e MyType
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)
import Dict exposing (Dict)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec e MyType
codec =
    Serialize.record MyType
        |> Serialize.field .fieldA (Serialize.dict Serialize.int Serialize.string)
        |> Serialize.finishRecord
"""
        , codeGenTest "nested record codec"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec e MyType
codec =
    Debug.todo ""
""" ]
            """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec e MyType
codec =
    Serialize.record MyType
        |> Serialize.field
            .fieldA
            (Serialize.record (\\field0 field1 -> { field0 = field0, field1 = field1 })
                |> Serialize.field .field0 Serialize.int
                |> Serialize.field .field1 Serialize.unit
                |> Serialize.finishRecord
            )
        |> Serialize.finishRecord
"""
        , codeGenTest "add nested codec" [ elmSerialize ] [ """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec e MyType
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec e MyType
codec =
    Serialize.record MyType |> Serialize.field .fieldA myOtherTypeCodec |> Serialize.finishRecord

myOtherTypeCodec : Codec e MyOtherType
myOtherTypeCodec =
    Serialize.record MyOtherType |> Serialize.field .fieldB Serialize.int |> Serialize.finishRecord
"""
        , codeGenTest "nested codec in another module"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec =
    Debug.todo ""
"""
            , """module B exposing (..)

type alias B = { field : Int }"""
            ]
            """module A exposing (..)

import Serialize exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec =
    Serialize.record A |> Serialize.field .field bCodec |> Serialize.finishRecord

bCodec : Codec e B
bCodec =
    Serialize.record B |> Serialize.field .field Serialize.int |> Serialize.finishRecord
"""
        , codeGenTest "twice nested codec in another module"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec =
    Debug.todo ""
"""
            , """module B exposing (..)

type alias B = { field1 : B2 }

type alias B2 = { field2 : Int }"""
            ]
            """module A exposing (..)

import Serialize exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec e A
codec =
    Serialize.record A |> Serialize.field .field bCodec |> Serialize.finishRecord

bCodec : Codec e B
bCodec =
    Serialize.record B |> Serialize.field .field1 b2Codec |> Serialize.finishRecord

b2Codec : Codec e B.B2
b2Codec =
    Serialize.record B.B2 |> Serialize.field .field2 Serialize.int |> Serialize.finishRecord
"""
        , codeGenTest "nested codec in list"
            [ elmSerialize ]
            [ """module A exposing (..)

import Serialize exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec e A
codec =
    Debug.todo ""
"""
            , """module B exposing (..)

type alias B = { field1 : Int }"""
            ]
            """module A exposing (..)

import Serialize exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec e A
codec =
    Serialize.record A |> Serialize.field .field (Serialize.list bCodec) |> Serialize.finishRecord

bCodec : Codec e B
bCodec =
    Serialize.record B |> Serialize.field .field1 Serialize.int |> Serialize.finishRecord
"""
        , -- This is not supported. In fact there are 2 features here that are still TODO:
          -- 1. Support for generics
          -- 2. Support for recursive datatypes
          Test.skip <| codeGenTest "codec with type parameter" [ elmSerialize ] [ """module A exposing (..)

import Serialize exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec e a -> Codec e (Tree a)
codec codecA = 
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec e a -> Codec e (Tree a)
codec codecA =
    Serialize.customType
     (\\a b value ->
         case value of
             Node data0 ->
                 a data0

             Leaf data0 ->
                 b data0
     )
        |> Serialize.variant1 Node (treeCodec (Debug.todo "Can't handle this"))
        |> Serialize.variant1 Leaf (Debug.todo "Can't handle this")
        |> Serialize.finishCustomType"""
        , codeGenTest "Add codec for type inside tuple" [ elmSerialize ] [ """module Schema exposing (..)

import Serialize exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec e A
codec =
    Debug.todo ""
""" ] """module Schema exposing (..)

import Serialize exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec e A
codec =
    Serialize.record A |> Serialize.field .a (Serialize.tuple bCodec Serialize.int) |> Serialize.finishRecord

bCodec : Codec e B
bCodec =
    Serialize.record B |> Serialize.field .b Serialize.int |> Serialize.finishRecord
"""
        , codeGenTest "Add codec for type inside nested record" [ elmSerialize ] [ """module Schema exposing (..)

import Serialize exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec e A
codec =
    Debug.todo ""
""" ] """module Schema exposing (..)

import Serialize exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec e A
codec =
    Serialize.record A
        |> Serialize.field
            .a
            (Serialize.record (\\b -> { b = b }) |> Serialize.field .b bCodec |> Serialize.finishRecord)
        |> Serialize.finishRecord

bCodec : Codec e B
bCodec =
    Serialize.record B |> Serialize.field .b Serialize.int |> Serialize.finishRecord
"""
        , codeGenTest "CaseId.codec not found regression test"
            [ elmSerialize ]
            [ """module Route exposing (..)
import Serialize exposing (Codec)
import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe CaseId)

routeCodec : Codec e Route
routeCodec =
    Debug.todo ""
"""
            , """module CaseId exposing (CaseId, codec)
import Serialize exposing (Codec)
type CaseId = CaseId String

codec : Codec e CaseId
codec =
    Serialize.string |> Serialize.map fromString toString
"""
            ]
            """module Route exposing (..)
import Serialize exposing (Codec)
import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe CaseId)

routeCodec : Codec e Route
routeCodec =
    Serialize.customType
        (\\myPagesRouteEncoder value ->
            case value of
                MyPagesRoute arg0 ->
                    myPagesRouteEncoder arg0
        )
        |> Serialize.variant1 MyPagesRoute (Serialize.maybe CaseId.codec)
        |> Serialize.finishCustomType
"""
        ]
