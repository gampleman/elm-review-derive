module SerializeCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (FakeDependency, codeGenTest)
import Test exposing (..)


elmSerialize : FakeDependency
elmSerialize =
    CodeGenerator.Test.fakeDependency
        { name = "MartinSStewart/elm-serialize"
        , dependencies = []
        , modules =
            [ { name = "Serialize"
              , values =
                    [ ( "array", "Serialize.Codec e a -> Serialize.Codec e (Array.Array a)" )
                    , ( "bool", "Serialize.Codec e Basics.Bool" )
                    , ( "byte", "Serialize.Codec e Basics.Int" )
                    , ( "bytes", "Serialize.Codec e Bytes.Bytes" )
                    , ( "customType", "match -> Serialize.CustomTypeCodec { youNeedAtLeastOneVariant : () } e match value" )
                    , ( "decodeFromBytes", "Serialize.Codec e a -> Bytes.Bytes -> Result.Result (Serialize.Error e) a" )
                    , ( "decodeFromJson", "Serialize.Codec e a -> Json.Encode.Value -> Result.Result (Serialize.Error e) a" )
                    , ( "decodeFromString", "Serialize.Codec e a -> String.String -> Result.Result (Serialize.Error e) a" )
                    , ( "dict", "Serialize.Codec e comparable -> Serialize.Codec e a -> Serialize.Codec e (Dict.Dict comparable a)" )
                    , ( "encodeToBytes", "Serialize.Codec e a -> a -> Bytes.Bytes" )
                    , ( "encodeToJson", "Serialize.Codec e a -> a -> Json.Encode.Value" )
                    , ( "encodeToString", "Serialize.Codec e a -> a -> String.String" )
                    , ( "enum", "a -> List.List a -> Serialize.Codec e a" )
                    , ( "field", "(a -> f) -> Serialize.Codec e f -> Serialize.RecordCodec e a (f -> b) -> Serialize.RecordCodec e a b" )
                    , ( "finishCustomType", "Serialize.CustomTypeCodec () e (a -> Serialize.VariantEncoder) a -> Serialize.Codec e a" )
                    , ( "finishRecord", "Serialize.RecordCodec e a a -> Serialize.Codec e a" )
                    , ( "float", "Serialize.Codec e Basics.Float" )
                    , ( "getJsonDecoder", "(e -> String.String) -> Serialize.Codec e a -> Json.Decode.Decoder a" )
                    , ( "int", "Serialize.Codec e Basics.Int" )
                    , ( "lazy", "(() -> Serialize.Codec e a) -> Serialize.Codec e a" )
                    , ( "list", "Serialize.Codec e a -> Serialize.Codec e (List.List a)" )
                    , ( "map", "(a -> b) -> (b -> a) -> Serialize.Codec e a -> Serialize.Codec e b" )
                    , ( "mapError", "(e1 -> e2) -> Serialize.Codec e1 a -> Serialize.Codec e2 a" )
                    , ( "mapValid", "(a -> Result.Result e b) -> (b -> a) -> Serialize.Codec e a -> Serialize.Codec e b" )
                    , ( "maybe", "Serialize.Codec e a -> Serialize.Codec e (Maybe.Maybe a)" )
                    , ( "record", "b -> Serialize.RecordCodec e a b" )
                    , ( "result", "Serialize.Codec e error -> Serialize.Codec e value -> Serialize.Codec e (Result.Result error value)" )
                    , ( "set", "Serialize.Codec e comparable -> Serialize.Codec e (Set.Set comparable)" )
                    , ( "string", "Serialize.Codec e String.String" )
                    , ( "triple", "Serialize.Codec e a -> Serialize.Codec e b -> Serialize.Codec e c -> Serialize.Codec e ( a, b, c )" )
                    , ( "tuple", "Serialize.Codec e a -> Serialize.Codec e b -> Serialize.Codec e ( a, b )" )
                    , ( "unit", "Serialize.Codec e ()" )
                    , ( "variant0", "v -> Serialize.CustomTypeCodec z e (Serialize.VariantEncoder -> a) v -> Serialize.CustomTypeCodec () e a v" )
                    , ( "variant1", "(a -> v) -> Serialize.Codec error a -> Serialize.CustomTypeCodec z error ((a -> Serialize.VariantEncoder) -> b) v -> Serialize.CustomTypeCodec () error b v" )
                    , ( "variant2", "(a -> b -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.CustomTypeCodec z error ((a -> b -> Serialize.VariantEncoder) -> c) v -> Serialize.CustomTypeCodec () error c v" )
                    , ( "variant3", "(a -> b -> c -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.Codec error c -> Serialize.CustomTypeCodec z error ((a -> b -> c -> Serialize.VariantEncoder) -> partial) v -> Serialize.CustomTypeCodec () error partial v" )
                    , ( "variant4", "(a -> b -> c -> d -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.Codec error c -> Serialize.Codec error d -> Serialize.CustomTypeCodec z error ((a -> b -> c -> d -> Serialize.VariantEncoder) -> partial) v -> Serialize.CustomTypeCodec () error partial v" )
                    , ( "variant5", "(a -> b -> c -> d -> e -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.Codec error c -> Serialize.Codec error d -> Serialize.Codec error e -> Serialize.CustomTypeCodec z error ((a -> b -> c -> d -> e -> Serialize.VariantEncoder) -> partial) v -> Serialize.CustomTypeCodec () error partial v" )
                    , ( "variant6", "(a -> b -> c -> d -> e -> f -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.Codec error c -> Serialize.Codec error d -> Serialize.Codec error e -> Serialize.Codec error f -> Serialize.CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> Serialize.VariantEncoder) -> partial) v -> Serialize.CustomTypeCodec () error partial v" )
                    , ( "variant7", "(a -> b -> c -> d -> e -> f -> g -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.Codec error c -> Serialize.Codec error d -> Serialize.Codec error e -> Serialize.Codec error f -> Serialize.Codec error g -> Serialize.CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> Serialize.VariantEncoder) -> partial) v -> Serialize.CustomTypeCodec () error partial v" )
                    , ( "variant8", "(a -> b -> c -> d -> e -> f -> g -> h -> v) -> Serialize.Codec error a -> Serialize.Codec error b -> Serialize.Codec error c -> Serialize.Codec error d -> Serialize.Codec error e -> Serialize.Codec error f -> Serialize.Codec error g -> Serialize.Codec error h -> Serialize.CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> h -> Serialize.VariantEncoder) -> partial) v -> Serialize.CustomTypeCodec () error partial v" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "Serialize code gen todo"
        [ codeGenTest "record codec" [ elmSerialize ] [] [ """module A exposing (..)

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
        , codeGenTest "qualified codec" [ elmSerialize ] [] [ """module A exposing (..)

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
            []
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
            []
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
            []
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
        , codeGenTest "maybe codec" [ elmSerialize ] [] [ """module A exposing (..)

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
        , codeGenTest "dict codec" [ elmSerialize ] [] [ """module A exposing (..)

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
            []
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
        , codeGenTest "add nested codec" [ elmSerialize ] [] [ """module A exposing (..)

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
            []
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
            []
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
            []
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
          codeGenTest "codec with type parameter" [ elmSerialize ] [] [ """module A exposing (..)

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
        (\\nodeEncoder leafEncoder value ->
            case value of
                Node arg0 ->
                    nodeEncoder arg0

                Leaf arg0 ->
                    leafEncoder arg0
        )
        |> Serialize.variant1 Node (Serialize.lazy (\\() -> codec codecA))
        |> Serialize.variant1 Leaf codecA
        |> Serialize.finishCustomType
"""
        , codeGenTest "Add codec for type inside tuple" [ elmSerialize ] [] [ """module Schema exposing (..)

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
        , codeGenTest "Add codec for type inside nested record" [ elmSerialize ] [] [ """module Schema exposing (..)

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
            []
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
