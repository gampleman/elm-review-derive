module MigrateCodeGenTest exposing (tests)

import CodeGen
import Review.Test
import Test exposing (..)


tests : Test
tests =
    describe "Migrate code gen tests"
        [ test "Custom type migration" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type MyType = A | B | C

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type MyType = A | B | C
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type MyType = A | B | C

migrateA : B.MyType -> MyType
migrateA old =
    case old of
        B.A ->
            A

        B.B ->
            B

        B.C ->
            C
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Custom type migration added variant" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type MyType = A | B | C

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type MyType = A | B
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type MyType = A | B | C

migrateA : B.MyType -> MyType
migrateA old =
    case old of
        B.A ->
            A

        B.B ->
            B
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Custom type migration removed variant" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type MyType = A | B

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type MyType = A | B | C
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type MyType = A | B

migrateA : B.MyType -> MyType
migrateA old =
    case old of
        B.A ->
            A

        B.B ->
            B

        B.C ->
            Debug.todo "Can't handle this"
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Custom type migration with parameters" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type MyType = B Int Int | A | C Float String

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type MyType = A Int | B Int Int | C String
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type MyType = B Int Int | A | C Float String

migrateA : B.MyType -> MyType
migrateA old =
    case old of
        B.A a ->
            Debug.todo "Can't handle this"

        B.B a b ->
            B a b

        B.C a ->
            Debug.todo "Can't handle this"
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Type alias non-migration" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : String }

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type alias MyType = { a : Int, b : String } 
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : String }

migrateA : B.MyType -> MyType
migrateA old =
    old
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Type alias new field" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : String, c : Float }

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type alias MyType = { a : Int, b : String }
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : String, c : Float }

migrateA : B.MyType -> MyType
migrateA old =
    { a = old.a, b = old.b, c = Debug.todo "Can't handle this" }
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Type alias missing field" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : String }

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type alias MyType = { a : Int, b : String, c : Float }
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : String }

migrateA : B.MyType -> MyType
migrateA old =
    Debug.todo "Can't handle this"
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Type alias changed field" <|
            \_ ->
                let
                    moduleA =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : Float }

migrateA : B.MyType -> MyType
migrateA = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module B exposing (..)
type alias MyType = { a : Int, b : String }
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module A exposing (..)

import B

type alias MyType = { a : Int, b : Float }

migrateA : B.MyType -> MyType
migrateA old =
    { a = old.a, b = Debug.todo "Can't handle this" }
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]

        --        , test "Type alias changed field but is migratable" <|
        --            \_ ->
        --                let
        --                    moduleA =
        --                        """module A exposing (..)
        --
        --import B
        --
        --type alias MyType = { a : Int, b : SubType }
        --
        --type SubType = A | B
        --
        --migrateA : B.MyType -> MyType
        --migrateA = Debug.todo ""
        --"""
        --                            |> String.replace "\u{000D}" ""
        --
        --                    moduleB =
        --                        """module B exposing (..)
        --type alias MyType = { a : Int, b : SubType }
        --
        --type SubType = A | B
        --"""
        --                            |> String.replace "\u{000D}" ""
        --
        --                    expected =
        --                        """module A exposing (..)
        --
        --import B
        --
        --type alias MyType = { a : Int, b : Float }
        --
        --type SubType = A | B
        --
        --migrateA : B.MyType -> MyType
        --migrateA old =
        --    { a = old.a, b = migrateSubType old.b }
        --
        --migrateSubType : B.SubType -> SubType
        --migrateSubType old =
        --    case old of
        --        B.A ->
        --            A
        --
        --        B.B ->
        --            B
        --"""
        --                in
        --                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
        --                    |> Review.Test.expectErrorsForModules
        --                        [ ( "A"
        --                          , [ Review.Test.error
        --                                { message = "Here's my attempt to complete this stub"
        --                                , details = [ "" ]
        --                                , under = "migrateA : B.MyType -> MyType\nmigrateA = Debug.todo \"\""
        --                                }
        --                                |> Review.Test.whenFixed expected
        --                            ]
        --                          )
        --                        ]
        , test "Regression test 1" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type FormCondition
    = Disabled
    | AlwaysAsk
    | Conditional Condition

migrateFormCondition : OldSchema.FormCondition -> FormCondition
migrateFormCondition old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type FormCondition
    = Disabled
    | AlwaysAsk
    | Conditional Condition
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type FormCondition
    = Disabled
    | AlwaysAsk
    | Conditional Condition

migrateFormCondition : OldSchema.FormCondition -> FormCondition
migrateFormCondition old =
    case old of
        OldSchema.Disabled ->
            Disabled

        OldSchema.AlwaysAsk ->
            AlwaysAsk

        OldSchema.Conditional a ->
            Conditional (migrateCondition a)
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateFormCondition : OldSchema.FormCondition -> FormCondition\nmigrateFormCondition old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Handle maybe" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A (Maybe MyType)

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type MyType
    = A (Maybe MyType)
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A (Maybe MyType)

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    case old of
        OldSchema.A a ->
            A (migrateMaybe migrateMyType a)
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Handle list" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A (List MyType)

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type MyType
    = A (List MyType)
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A (List MyType)

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    case old of
        OldSchema.A a ->
            A (migrateList migrateMyType a)
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Handle result" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A (Result Error MyType)

type alias Error = {}

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type MyType
    = A (Result Error MyType)

type alias Error = {}
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A (Result Error MyType)

type alias Error = {}

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    case old of
        OldSchema.A a ->
            A (migrateResult migrateError migrateMyType a)
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Handle record nested in variant" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A { fieldA : MyType }

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type MyType
    = A { fieldA : MyType }
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type MyType
    = A { fieldA : MyType }

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    case old of
        OldSchema.A a ->
            A { fieldA = migrateMyType a.fieldA }
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]

        --        , test "Handle non-migratable record nested in record field" <|
        --            \_ ->
        --                let
        --                    moduleA =
        --                        """module Schema exposing (..)
        --
        --import OldSchema
        --
        --type alias MyType =
        --    { field1 : { fieldA : Int } }
        --
        --migrateMyType : OldSchema.MyType -> MyType
        --migrateMyType old = Debug.todo ""
        --"""
        --                            |> String.replace "\u{000D}" ""
        --
        --                    moduleB =
        --                        """module OldSchema exposing (..)
        --
        --type alias MyType =
        --    { field1 : { fieldA : Float } }
        --"""
        --                            |> String.replace "\u{000D}" ""
        --
        --                    expected =
        --                        """module Schema exposing (..)
        --
        --import OldSchema
        --
        --type alias MyType =
        --    { field1 : { fieldA : Int } }
        --
        --migrateMyType : OldSchema.MyType -> MyType
        --migrateMyType old =
        --    { field1 = { fieldA = Debug.todo "Can't handle this" } }
        --"""
        --                in
        --                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
        --                    |> Review.Test.expectErrorsForModules
        --                        [ ( "Schema"
        --                          , [ Review.Test.error
        --                                { message = "Here's my attempt to complete this stub"
        --                                , details = [ "" ]
        --                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
        --                                }
        --                                |> Review.Test.whenFixed expected
        --                            ]
        --                          )
        --                        ]
        , test "Handle migratable record nested in record field" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type alias MyType =
    { field1 : { fieldA : Foo } }

type Foo = Foo

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type alias MyType =
    { field1 : { fieldA : Foo } }

type Foo = Foo
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type alias MyType =
    { field1 : { fieldA : Foo } }

type Foo = Foo

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    { field1 = { fieldA = migrateFoo old.field1.fieldA } }
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Handle migratable tuple nested in record field" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type alias MyType =
    { field1 : (Int, Foo) }

type Foo = Foo

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type alias MyType =
    { field1 : (Int, Foo) }

type Foo = Foo
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type alias MyType =
    { field1 : (Int, Foo) }

type Foo = Foo

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    { field1 = migrateTuple identity migrateFoo old.field1 }
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        , test "Handle migratable triple nested in record field" <|
            \_ ->
                let
                    moduleA =
                        """module Schema exposing (..)

import OldSchema

type alias MyType =
    { field1 : (Int, Foo, String) }

type Foo = Foo

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old = Debug.todo ""
"""
                            |> String.replace "\u{000D}" ""

                    moduleB =
                        """module OldSchema exposing (..)

type alias MyType =
    { field1 : (Int, Foo, Float) }

type Foo = Foo
"""
                            |> String.replace "\u{000D}" ""

                    expected =
                        """module Schema exposing (..)

import OldSchema

type alias MyType =
    { field1 : (Int, Foo, String) }

type Foo = Foo

migrateMyType : OldSchema.MyType -> MyType
migrateMyType old =
    { field1 = migrateTriple identity migrateFoo (Debug.todo "Can't handle this") old.field1 }
"""
                in
                Review.Test.runOnModules CodeGen.rule [ moduleA, moduleB ]
                    |> Review.Test.expectErrorsForModules
                        [ ( "Schema"
                          , [ Review.Test.error
                                { message = "Here's my attempt to complete this stub"
                                , details = [ "" ]
                                , under = "migrateMyType : OldSchema.MyType -> MyType\nmigrateMyType old = Debug.todo \"\""
                                }
                                |> Review.Test.whenFixed expected
                            ]
                          )
                        ]
        ]
