module MiniBillCodecCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (FakeDependency, codeGenTest)
import Test exposing (Test, describe)


elmCodec : FakeDependency
elmCodec =
    CodeGenerator.Test.fakeDependency
        { name = "miniBill/elm-codec"
        , dependencies = []
        , modules =
            [ { name = "Codec"
              , values =
                    [ ( "decoder", "Codec.Codec a -> Json.Decode.Decoder a" )
                    , ( "decodeString", "Codec.Codec a -> String -> Result Json.Decode.Error a" )
                    , ( "decodeValue", "Codec.Codec a -> Json.Decode.Value -> Result Json.Decode.Error a" )
                    , ( "encoder", "Codec.Codec a -> a -> Json.Decode.Value" )
                    , ( "encodeToString", "Int -> Codec.Codec a -> a -> String" )
                    , ( "encodeToValue", "Codec.Codec a -> a -> Json.Decode.Value" )
                    , ( "build", "(a -> Json.Decode.Value) -> Json.Decode.Decoder a -> Codec.Codec a" )
                    , ( "string", "Codec.Codec String" )
                    , ( "bool", "Codec.Codec Bool" )
                    , ( "int", "Codec.Codec Int" )
                    , ( "float", "Codec.Codec Float" )
                    , ( "char", "Codec.Codec Char" )
                    , ( "enum", "Codec.Codec a -> List.List ( a, b ) -> Codec.Codec b" )
                    , ( "composite", "((b -> Json.Decode.Value) -> (a -> Json.Decode.Value)) -> (Json.Decode.Decoder b -> Json.Decode.Decoder a) -> Codec.Codec b -> Codec.Codec a" )
                    , ( "maybe", "Codec.Codec a -> Codec.Codec (Maybe.Maybe a)" )
                    , ( "nullable", "Codec.Codec a -> Codec.Codec (Maybe.Maybe a)" )
                    , ( "list", "Codec.Codec a -> Codec.Codec (List.List a)" )
                    , ( "array", "Codec.Codec a -> Codec.Codec (Array.Array a)" )
                    , ( "dict", "Codec.Codec a -> Codec.Codec (Dict.Dict String a)" )
                    , ( "set", "Codec.Codec comparable -> Codec.Codec (Set.Set comparable)" )
                    , ( "tuple", "Codec.Codec a -> Codec.Codec b -> Codec.Codec ( a, b )" )
                    , ( "triple", "Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.Codec ( a, b, c )" )
                    , ( "result", "Codec.Codec error -> Codec.Codec value -> Codec.Codec (Result.Result error value)" )
                    , ( "object", "b -> Codec.ObjectCodec a b" )
                    , ( "field", "String -> (a -> f) -> Codec.Codec f -> Codec.ObjectCodec a (f -> b) -> Codec.ObjectCodec a b" )
                    , ( "maybeField", "String -> (a -> Maybe.Maybe f) -> Codec.Codec f -> Codec.ObjectCodec a (Maybe.Maybe f -> b) -> Codec.ObjectCodec a b" )
                    , ( "optionalField", "String -> (a -> Maybe.Maybe f) -> Codec.Codec f -> Codec.ObjectCodec a (Maybe.Maybe f -> b) -> Codec.ObjectCodec a b" )
                    , ( "optionalNullableField", "String -> (a -> Maybe.Maybe f) -> Codec.Codec f -> Codec.ObjectCodec a (Maybe.Maybe f -> b) -> Codec.ObjectCodec a b" )
                    , ( "nullableField", "String -> (a -> Maybe.Maybe f) -> Codec.Codec f -> Codec.ObjectCodec a (Maybe.Maybe f -> b) -> Codec.ObjectCodec a b" )
                    , ( "buildObject", "Codec.ObjectCodec a a -> Codec.Codec a" )
                    , ( "custom", "match -> Codec.CustomCodec match value" )
                    , ( "variant", "String -> ((List.List Json.Decode.Value -> Json.Decode.Value) -> a) -> Json.Decode.Decoder v -> Codec.CustomCodec (a -> b) v -> Codec.CustomCodec b v" )
                    , ( "variant0", "String -> v -> Codec.CustomCodec (Json.Decode.Value -> a) v -> Codec.CustomCodec a v" )
                    , ( "variant1", "String -> (a -> v) -> Codec.Codec a -> Codec.CustomCodec ((a -> Json.Decode.Value) -> b) v -> Codec.CustomCodec b v" )
                    , ( "variant2", "String -> (a -> b -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.CustomCodec ((a -> b -> Json.Decode.Value) -> c) v -> Codec.CustomCodec c v" )
                    , ( "variant3", "String -> (a -> b -> c -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.CustomCodec ((a -> b -> c -> Json.Decode.Value) -> partial) v -> Codec.CustomCodec partial v" )
                    , ( "variant4", "String -> (a -> b -> c -> d -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.Codec d -> Codec.CustomCodec ((a -> b -> c -> d -> Json.Decode.Value) -> partial) v -> Codec.CustomCodec partial v" )
                    , ( "variant5", "String -> (a -> b -> c -> d -> e -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.Codec d -> Codec.Codec e -> Codec.CustomCodec ((a -> b -> c -> d -> e -> Json.Decode.Value) -> partial) v -> Codec.CustomCodec partial v" )
                    , ( "variant6", "String -> (a -> b -> c -> d -> e -> f -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.Codec d -> Codec.Codec e -> Codec.Codec f -> Codec.CustomCodec ((a -> b -> c -> d -> e -> f -> Json.Decode.Value) -> partial) v -> Codec.CustomCodec partial v" )
                    , ( "variant7", "String -> (a -> b -> c -> d -> e -> f -> g -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.Codec d -> Codec.Codec e -> Codec.Codec f -> Codec.Codec g -> Codec.CustomCodec ((a -> b -> c -> d -> e -> f -> g -> Json.Decode.Value) -> partial) v -> Codec.CustomCodec partial v" )
                    , ( "variant8", "String -> (a -> b -> c -> d -> e -> f -> g -> h -> v) -> Codec.Codec a -> Codec.Codec b -> Codec.Codec c -> Codec.Codec d -> Codec.Codec e -> Codec.Codec f -> Codec.Codec g -> Codec.Codec h -> Codec.CustomCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Json.Decode.Value) -> partial) v -> Codec.CustomCodec partial v" )
                    , ( "buildCustom", "Codec.CustomCodec (a -> Json.Decode.Value) a -> Codec.Codec a" )
                    , ( "oneOf", "Codec.Codec a -> List.List (Codec.Codec a) -> Codec.Codec a" )
                    , ( "map", "(a -> b) -> (b -> a) -> Codec.Codec a -> Codec.Codec b" )
                    , ( "fail", "String -> Codec.Codec a" )
                    , ( "andThen", "(a -> Codec.Codec b) -> (b -> a) -> Codec.Codec a -> Codec.Codec b" )
                    , ( "recursive", "(Codec.Codec a -> Codec.Codec a) -> Codec.Codec a" )
                    , ( "succeed", "a -> Codec.Codec a" )
                    , ( "constant", "a -> Codec.Codec a" )
                    , ( "lazy", "(() -> Codec.Codec a) -> Codec.Codec a" )
                    , ( "value", "Codec.Codec Json.Decode.Value" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "Codec code gen todo"
        [ codeGenTest "record codec" [ elmCodec ] [] [ """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int, fieldB : String, fieldC : B }
type alias B = { fieldD : Float }

codec : Codec A
codec =
    Debug.todo ""
""" ] """module A exposing (..)

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
    Codec.object B |> Codec.field "fieldD" .fieldD Codec.float |> Codec.buildObject
"""
        , codeGenTest "qualified codec" [ elmCodec ] [] [ """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int }

codec : Codec.Codec A
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Codec exposing (Codec)

type alias A = { fieldA : Int }

codec : Codec.Codec A
codec =
    Codec.object A |> Codec.field "fieldA" .fieldA Codec.int |> Codec.buildObject
"""
        , codeGenTest "custom type codec"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String String

codec : Codec MyType
codec =
    Debug.todo ""
""" ]
            """module A exposing (..)

import Codec exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String String

codec : Codec MyType
codec =
    Codec.custom
        (\\variantAEncoder variantBEncoder variantCEncoder value ->
            case value of
                VariantA ->
                    variantAEncoder

                VariantB arg0 ->
                    variantBEncoder arg0

                VariantC arg0 arg1 ->
                    variantCEncoder arg0 arg1
        )
        |> Codec.variant0 "VariantA" VariantA
        |> Codec.variant1 "VariantB" VariantB Codec.int
        |> Codec.variant2 "VariantC" VariantC Codec.string Codec.string
        |> Codec.buildCustom
"""
        , codeGenTest "custom type in another module"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)
import OtherModule

codec : Codec OtherModule.MyType
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

import Codec exposing (Codec)
import OtherModule

codec : Codec OtherModule.MyType
codec =
    Codec.custom
        (\\variantAEncoder variantBEncoder variantCEncoder value ->
            case value of
                OtherModule.VariantA ->
                    variantAEncoder

                OtherModule.VariantB arg0 ->
                    variantBEncoder arg0

                OtherModule.VariantC arg0 ->
                    variantCEncoder arg0
        )
        |> Codec.variant0 "VariantA" OtherModule.VariantA
        |> Codec.variant1 "VariantB" OtherModule.VariantB Codec.int
        |> Codec.variant1 "VariantC" OtherModule.VariantC Codec.string
        |> Codec.buildCustom
"""
        , codeGenTest "reuse existing codec in another module"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec A
codec =
    Debug.todo ""
"""
            , """module OtherModule exposing (..)

import Codec exposing (Codec)

type MyType
    = VariantA
    | VariantB Int
    | VariantC String

codec : Codec MyType
codec  =
    Codec.custom
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
        |> Codec.variant1 "VariantB" VariantB Codec.int
        |> Codec.variant1 "VariantC" VariantC Codec.string
        |> Codec.buildCustom"""
            ]
            """module A exposing (..)

import Codec exposing (Codec)
import OtherModule exposing (MyType)

type alias A = { field : MyType }

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field OtherModule.codec |> Codec.buildObject
"""
        , codeGenTest "maybe codec" [ elmCodec ] [] [ """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec MyType
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : Maybe Int }

codec : Codec MyType
codec =
    Codec.object MyType |> Codec.field "fieldA" .fieldA (Codec.nullable Codec.int) |> Codec.buildObject
"""
        , codeGenTest "dict codec" [ elmCodec ] [] [ """module A exposing (..)

import Codec exposing (Codec)
import Dict exposing (Dict)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec MyType
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Codec exposing (Codec)
import Dict exposing (Dict)

type alias MyType =
    { fieldA : Dict Int String
    }

codec : Codec MyType
codec =
    Codec.object MyType
        |> Codec.field
            "fieldA"
            .fieldA
            (Codec.map Dict.fromList Dict.toList (Codec.list (Codec.tuple Codec.int Codec.string)))
        |> Codec.buildObject
"""
        , codeGenTest "nested record codec"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec MyType
codec =
    Debug.todo ""
""" ]
            """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : { field0 : Int, field1 : () } }

codec : Codec MyType
codec =
    Codec.object MyType
        |> Codec.field
            "fieldA"
            .fieldA
            (Codec.object (\\field0 field1 -> { field0 = field0, field1 = field1 })
                |> Codec.field "field0" .field0 Codec.int
                |> Codec.field "field1" .field1 Codec.unit
                |> Codec.buildObject
            )
        |> Codec.buildObject
"""
        , codeGenTest "add nested codec" [ elmCodec ] [] [ """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec MyType
codec =
    Debug.todo ""
""" ] """module A exposing (..)

import Codec exposing (Codec)

type alias MyType = { fieldA : MyOtherType }

type alias MyOtherType = { fieldB : Int }

codec : Codec MyType
codec =
    Codec.object MyType |> Codec.field "fieldA" .fieldA myOtherTypeCodec |> Codec.buildObject

myOtherTypeCodec : Codec MyOtherType
myOtherTypeCodec =
    Codec.object MyOtherType |> Codec.field "fieldB" .fieldB Codec.int |> Codec.buildObject
"""
        , codeGenTest "nested codec in another module"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
codec =
    Debug.todo ""
"""
            , """module B exposing (..)

type alias B = { field : Int }"""
            ]
            """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field bCodec |> Codec.buildObject

bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "field" .field Codec.int |> Codec.buildObject
"""
        , codeGenTest "twice nested codec in another module"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
codec =
    Debug.todo ""
"""
            , """module B exposing (..)

type alias B = { field1 : B2 }

type alias B2 = { field2 : Int }"""
            ]
            """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : B }

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field bCodec |> Codec.buildObject

bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "field1" .field1 b2Codec |> Codec.buildObject

b2Codec : Codec B.B2
b2Codec =
    Codec.object B.B2 |> Codec.field "field2" .field2 Codec.int |> Codec.buildObject
"""
        , codeGenTest "nested codec in list"
            [ elmCodec ]
            []
            [ """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec A
codec =
    Debug.todo ""
"""
            , """module B exposing (..)

type alias B = { field1 : Int }"""
            ]
            """module A exposing (..)

import Codec exposing (Codec)
import B exposing (B)

type alias A = { field : List B }

codec : Codec A
codec =
    Codec.object A |> Codec.field "field" .field (Codec.list bCodec) |> Codec.buildObject

bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "field1" .field1 Codec.int |> Codec.buildObject
"""
        , -- This is not supported. In fact there are 2 features here that are still TODO:
          -- 1. Support for generics
          -- 2. Support for recursive datatypes
          codeGenTest "codec with type parameter" [ elmCodec ] [] [ """module A exposing (..)

import Codec exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec a -> Codec (Tree a)
codec codecA =
    Debug.todo ""
""" ] """module A exposing (..)

import Codec exposing (Codec)

type Tree a
    = Node (Tree a)
    | Leaf a

codec : Codec a -> Codec (Tree a)
codec codecA =
    Codec.custom
        (\\nodeEncoder leafEncoder value ->
            case value of
                Node arg0 ->
                    nodeEncoder arg0

                Leaf arg0 ->
                    leafEncoder arg0
        )
        |> Codec.variant1 "Node" Node (Codec.lazy (\\() -> codec codecA))
        |> Codec.variant1 "Leaf" Leaf codecA
        |> Codec.buildCustom
"""
        , codeGenTest "Add codec for type inside tuple" [ elmCodec ] [] [ """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec A
codec =
    Debug.todo ""
""" ] """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : ( B, Int ) }

type alias B = { b : Int }

codec : Codec A
codec =
    Codec.object A |> Codec.field "a" .a (Codec.tuple bCodec Codec.int) |> Codec.buildObject

bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "b" .b Codec.int |> Codec.buildObject
"""
        , codeGenTest "Add codec for type inside nested record" [ elmCodec ] [] [ """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec A
codec =
    Debug.todo ""
""" ] """module Schema exposing (..)

import Codec exposing (Codec)

type alias A =
   { a : { b : B } }

type alias B = { b : Int }

codec : Codec A
codec =
    Codec.object A
        |> Codec.field "a" .a (Codec.object (\\b -> { b = b }) |> Codec.field "b" .b bCodec |> Codec.buildObject)
        |> Codec.buildObject

bCodec : Codec B
bCodec =
    Codec.object B |> Codec.field "b" .b Codec.int |> Codec.buildObject
"""
        , codeGenTest "CaseId.codec not found regression test"
            [ elmCodec ]
            []
            [ """module Route exposing (..)
import Codec exposing (Codec)
import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe.Maybe CaseId)

routeCodec : Codec Route
routeCodec =
    Debug.todo ""
"""
            , """module CaseId exposing (CaseId, codec)
import Codec exposing (Codec)
type CaseId = CaseId String

codec : Codec CaseId
codec =
    Codec.string |> Codec.map fromString toString
"""
            ]
            """module Route exposing (..)
import Codec exposing (Codec)
import CaseId exposing (CaseId)

type Route
    = MyPagesRoute (Maybe.Maybe CaseId)

routeCodec : Codec Route
routeCodec =
    Codec.custom
        (\\myPagesRouteEncoder value ->
            case value of
                MyPagesRoute arg0 ->
                    myPagesRouteEncoder arg0
        )
        |> Codec.variant1 "MyPagesRoute" MyPagesRoute (Codec.nullable CaseId.codec)
        |> Codec.buildCustom
"""
        ]
