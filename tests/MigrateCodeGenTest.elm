module MigrateCodeGenTest exposing (tests)

import CodeGen
import Elm.Project
import Expect
import Json.Decode
import Review.Project
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
        , only <|
            test "Generate migration for sheep game" <|
                \_ ->
                    case Json.Decode.decodeString Elm.Project.decoder elmJsonText of
                        Ok elmJson ->
                            Review.Test.runOnModulesWithProjectData
                                (Review.Project.addElmJson
                                    { path = "elm.json"
                                    , raw = elmJsonText
                                    , project = elmJson
                                    }
                                    Review.Project.new
                                )
                                CodeGen.rule
                                (List.map
                                    (String.replace "\u{000D}" "")
                                    [ migrateModule
                                    , colorIndexModule
                                    , idModule
                                    , questionWithGroupModule
                                    , typesModule
                                    , userModule
                                    , oldColorIndexModule
                                    , oldIdModule
                                    , oldTypesModule
                                    , oldUserModule
                                    ]
                                )
                                |> Review.Test.expectErrorsForModules
                                    [ ( "Evergreen.Migrate.V9"
                                      , [ Review.Test.error
                                            { message = "Here's my attempt to complete this stub"
                                            , details = [ "" ]
                                            , under = "frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg\nfrontendModel old =\n    Unimplemented"
                                            }
                                            |> Review.Test.whenFixed migrateModule
                                        ]
                                      )
                                    ]

                        Err _ ->
                            Expect.fail "Invalid elm json"
        ]


elmJsonText =
    """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "MartinSStewart/elm-nonempty-string": "2.0.0",
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0",
            "elm/url": "1.0.0",
            "elm-community/list-extra": "8.5.1",
            "erlandsona/assoc-set": "1.1.0",
            "folkertdev/elm-sha2": "1.0.0",
            "ianmackenzie/elm-units": "2.9.0",
            "lamdera/codecs": "1.0.0",
            "lamdera/core": "1.0.0",
            "lamdera/program-test": "1.0.0",
            "mdgriffith/elm-ui": "1.1.8",
            "mgold/elm-nonempty-list": "4.2.0",
            "pzp1997/assoc-list": "1.0.0"
        },
        "indirect": {
            "danfishgold/base64-bytes": "1.1.0",
            "elm/bytes": "1.0.8",
            "elm/file": "1.0.5",
            "elm/http": "2.0.0",
            "elm/json": "1.1.3",
            "elm/parser": "1.1.0",
            "elm/random": "1.0.0",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.2",
            "elm-community/basics-extra": "4.1.0",
            "elm-explorations/test": "1.2.2",
            "elm-explorations/webgl": "1.1.3",
            "justinmimbs/date": "3.2.1",
            "rtfeldman/elm-hex": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""


migrateModule =
    """module Evergreen.Migrate.V9 exposing (..)

import Evergreen.V5.Types as Old
import Evergreen.V9.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    Unimplemented


--backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
--backendModel old =
--    Unimplemented
--
--
--frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
--frontendMsg old =
--    Unimplemented
--
--
--toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
--toBackend old =
--    Unimplemented
--
--
--backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
--backendMsg old =
--    MsgUnchanged
--
--
--toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
--toFrontend old =
--    Unimplemented
"""


oldColorIndexModule =
    """module Evergreen.V5.ColorIndex exposing (..)


type ColorIndex
    = Red
    | LightRed
    | Green
    | LightGreen
    | Blue
    | LightBlue
    | White
    | Gray
    | Orange
    | Brown
    | Yellow
    | Purple
    | LightPurple
    | Black
"""


colorIndexModule =
    """module Evergreen.V9.ColorIndex exposing (..)


type ColorIndex
    = Red
    | LightRed
    | Green
    | LightGreen
    | Blue
    | LightBlue
    | White
    | Gray
    | Orange
    | Brown
    | Yellow
    | Purple
    | LightPurple
    | Black
"""


oldIdModule =
    """module Evergreen.V5.Id exposing (..)


type GameId
    = GameId Never


type Id a
    = Id String


type UserId
    = UserId Never


type HostLink
    = HostLink Never
"""


idModule =
    """module Evergreen.V9.Id exposing (..)


type GameId
    = GameId Never


type Id a
    = Id String


type UserId
    = UserId Never


type HostLink
    = HostLink Never
"""


questionWithGroupModule =
    """module Evergreen.V9.QuestionWithGroup exposing (..)

import AssocList
import Evergreen.V9.Id
import String.Nonempty


type alias QuestionWithGroup =
    { question : String.Nonempty.NonemptyString
    , answers :
        AssocList.Dict
            (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)
            { answer : String.Nonempty.NonemptyString
            , group : String
            }
    , notes : String
    }
"""


oldTypesModule =
    """module Evergreen.V5.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Dom
import Effect.Browser.Navigation
import Effect.Lamdera
import Evergreen.V5.ColorIndex
import Evergreen.V5.Id
import Evergreen.V5.User
import List.Nonempty
import String.Nonempty
import Url


type alias Question =
    { question : String.Nonempty.NonemptyString
    , answers : AssocList.Dict (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) String.Nonempty.NonemptyString
    }


type alias OpenSession_ =
    { host : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
    , questions : List.Nonempty.Nonempty Question
    , hostLink : Evergreen.V5.Id.Id Evergreen.V5.Id.HostLink
    }


type alias QuestionWithGroup =
    { question : String.Nonempty.NonemptyString
    , answers :
        AssocList.Dict
            (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId)
            { answer : String.Nonempty.NonemptyString
            , group : String
            }
    , notes : String
    }


type alias AddingNotesAndGroups_ =
    { host : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
    , questions : List.Nonempty.Nonempty QuestionWithGroup
    , hostLink : Evergreen.V5.Id.Id Evergreen.V5.Id.HostLink
    }


type alias ClosedSession_ =
    { host : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
    , questions : List.Nonempty.Nonempty QuestionWithGroup
    , questionsRevealed : Int
    , hostLink : Evergreen.V5.Id.Id Evergreen.V5.Id.HostLink
    }


type Game
    = EditingQuestions
        { host : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
        , questions : List String
        }
    | OpenSession OpenSession_
    | AddingNotesAndGroups AddingNotesAndGroups_
    | ClosedSession ClosedSession_


type alias AnsweringQuestions_ =
    { questions :
        List.Nonempty.Nonempty
            { question : String.Nonempty.NonemptyString
            , answer : String
            }
    , saving : Bool
    , debounceCount : Int
    }


type GameState
    = HostingGame Game
    | AnsweringQuestions AnsweringQuestions_
    | WaitingForResults
    | ViewingResults
        { hostId : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
        , questions : List.Nonempty.Nonempty QuestionWithGroup
        , questionsRevealed : Int
        , newQuestionAlert : Bool
        , viewport : Maybe Effect.Browser.Dom.Viewport
        }


type alias LoadedGame_ =
    { userId : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
    , gameId : Evergreen.V5.Id.Id Evergreen.V5.Id.GameId
    , gameState : GameState
    , key : Effect.Browser.Navigation.Key
    , userCache : AssocList.Dict (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) Evergreen.V5.User.User
    }


type FrontendModel
    = CreatingNewGame Effect.Browser.Navigation.Key
    | LoadingGame (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Effect.Browser.Navigation.Key
    | LoadedGame LoadedGame_
    | GameNotFound Effect.Browser.Navigation.Key


type alias BackendModel =
    { games : AssocList.Dict (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Game
    , sessions : AssocList.Dict Effect.Lamdera.SessionId (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId)
    , users : AssocList.Dict (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) Evergreen.V5.User.User
    , idCounter : Int
    }


type LoadedGameMsg
    = TypedQuestion Int String
    | TypedAnswer Int String
    | PressedAddQuestion
    | PressedRemoveQuestion Int
    | PressedSubmitQuestions
    | PressedHideCurrentQuestion
    | PressedShowNextQuestion
    | TypedName String
    | PressedColor Evergreen.V5.ColorIndex.ColorIndex
    | PressedLockAnswers
    | DebounceFinished Int
    | TypedNotes Int String
    | TypedGroup (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) Int String
    | PressedRevealScores
    | TimeUpdate
    | ViewportUpdate Effect.Browser.Dom.Viewport


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadedGameMsg LoadedGameMsg


type ToBackend
    = HostDataRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) (Maybe (Evergreen.V5.Id.Id Evergreen.V5.Id.HostLink))
    | CreateGameRequest
    | SetQuestions (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) (List String)
    | FinishEditQuestionsRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) (List.Nonempty.Nonempty String.Nonempty.NonemptyString)
    | SetAnswersRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Int (List.Nonempty.Nonempty String)
    | SetNotes (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Int String
    | SetUserData Evergreen.V5.User.User
    | LockAnswersRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId)
    | FinishNotesAndPointsRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId)
    | SetQuestionsRevealed (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Int
    | SetGroup
        { gameId : Evergreen.V5.Id.Id Evergreen.V5.Id.GameId
        , userId : Evergreen.V5.Id.Id Evergreen.V5.Id.UserId
        , questionIndex : Int
        , group : String
        }


type BackendMsg
    = OnDisconnect Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type LoadedGameToFrontend
    = LockAnswersResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId)
    | UpdateAnswers (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) (List.Nonempty.Nonempty String)
    | QuestionsRevealedChanged (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Int
    | FinishNotesAndPointsResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) (List.Nonempty.Nonempty QuestionWithGroup)
    | FinishEditQuestionsResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) (Evergreen.V5.Id.Id Evergreen.V5.Id.HostLink) (List.Nonempty.Nonempty String.Nonempty.NonemptyString)
    | SetAnswersResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId) Int


type ToFrontend
    = CreateGameResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) Evergreen.V5.User.User (Evergreen.V5.Id.Id Evergreen.V5.Id.GameId)
    | HostDataResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) (AssocList.Dict (Evergreen.V5.Id.Id Evergreen.V5.Id.UserId) Evergreen.V5.User.User) (Maybe GameState)
    | LoadedGameToFrontend LoadedGameToFrontend
"""


typesModule =
    """module Evergreen.V9.Types exposing (..)

import AssocList
import AssocSet
import Browser
import Effect.Browser.Dom
import Effect.Browser.Navigation
import Effect.Lamdera
import Evergreen.V9.ColorIndex
import Evergreen.V9.Id
import Evergreen.V9.QuestionWithGroup
import Evergreen.V9.User
import List.Nonempty
import String.Nonempty
import Url


type alias Question =
    { question : String.Nonempty.NonemptyString
    , answers : AssocList.Dict (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) String.Nonempty.NonemptyString
    }


type alias OpenSession_ =
    { host : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
    , questions : List.Nonempty.Nonempty Question
    , hostLink : Evergreen.V9.Id.Id Evergreen.V9.Id.HostLink
    }


type alias AddingNotesAndGroups_ =
    { host : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
    , questions : List.Nonempty.Nonempty Evergreen.V9.QuestionWithGroup.QuestionWithGroup
    , hostLink : Evergreen.V9.Id.Id Evergreen.V9.Id.HostLink
    , hiddenUsers : AssocSet.Set (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)
    }


type alias ClosedSession_ =
    { host : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
    , questions : List.Nonempty.Nonempty Evergreen.V9.QuestionWithGroup.QuestionWithGroup
    , questionsRevealed : Int
    , hostLink : Evergreen.V9.Id.Id Evergreen.V9.Id.HostLink
    }


type Game
    = EditingQuestions
        { host : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
        , questions : List String
        }
    | OpenSession OpenSession_
    | AddingNotesAndGroups AddingNotesAndGroups_
    | ClosedSession ClosedSession_


type alias AnsweringQuestions_ =
    { questions :
        List.Nonempty.Nonempty
            { question : String.Nonempty.NonemptyString
            , answer : String
            }
    , saving : Bool
    , debounceCount : Int
    }


type GameState
    = HostingGame Game
    | AnsweringQuestions AnsweringQuestions_
    | WaitingForResults
    | ViewingResults
        { hostId : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
        , questions : List.Nonempty.Nonempty Evergreen.V9.QuestionWithGroup.QuestionWithGroup
        , questionsRevealed : Int
        , newQuestionAlert : Bool
        , viewport : Maybe Effect.Browser.Dom.Viewport
        }


type alias LoadedGame_ =
    { userId : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
    , gameId : Evergreen.V9.Id.Id Evergreen.V9.Id.GameId
    , gameState : GameState
    , key : Effect.Browser.Navigation.Key
    , userCache : AssocList.Dict (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) Evergreen.V9.User.User
    }


type FrontendModel
    = CreatingNewGame Effect.Browser.Navigation.Key
    | LoadingGame (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Effect.Browser.Navigation.Key
    | LoadedGame LoadedGame_
    | GameNotFound Effect.Browser.Navigation.Key


type alias BackendModel =
    { games : AssocList.Dict (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Game
    , sessions : AssocList.Dict Effect.Lamdera.SessionId (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)
    , users : AssocList.Dict (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) Evergreen.V9.User.User
    , idCounter : Int
    }


type LoadedGameMsg
    = TypedQuestion Int String
    | TypedAnswer Int String
    | PressedAddQuestion
    | PressedRemoveQuestion Int
    | PressedSubmitQuestions
    | PressedHideCurrentQuestion
    | PressedShowNextQuestion
    | TypedName String
    | PressedColor Evergreen.V9.ColorIndex.ColorIndex
    | PressedLockAnswers
    | DebounceFinished Int
    | TypedNotes Int String
    | TypedGroup (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) Int String
    | PressedRevealScores
    | TimeUpdate
    | ViewportUpdate Effect.Browser.Dom.Viewport
    | PressedHideUser (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)
    | PressedUnhideUser (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadedGameMsg LoadedGameMsg


type ToBackend
    = HostDataRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (Maybe (Evergreen.V9.Id.Id Evergreen.V9.Id.HostLink))
    | CreateGameRequest
    | SetQuestions (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (List String)
    | FinishEditQuestionsRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (List.Nonempty.Nonempty String.Nonempty.NonemptyString)
    | SetAnswersRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Int (List.Nonempty.Nonempty String)
    | SetNotes (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Int String
    | SetUserData Evergreen.V9.User.User
    | LockAnswersRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId)
    | FinishNotesAndPointsRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId)
    | SetQuestionsRevealed (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Int
    | SetGroup
        { gameId : Evergreen.V9.Id.Id Evergreen.V9.Id.GameId
        , userId : Evergreen.V9.Id.Id Evergreen.V9.Id.UserId
        , questionIndex : Int
        , group : String
        }
    | HideUserRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)
    | UnhideUserRequest (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId)


type BackendMsg
    = OnDisconnect Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type LoadedGameToFrontend
    = LockAnswersResponse (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId)
    | UpdateAnswers (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) (List.Nonempty.Nonempty String)
    | QuestionsRevealedChanged (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Int
    | FinishNotesAndPointsResponse (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) (List.Nonempty.Nonempty Evergreen.V9.QuestionWithGroup.QuestionWithGroup)
    | FinishEditQuestionsResponse (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) (Evergreen.V9.Id.Id Evergreen.V9.Id.HostLink) (List.Nonempty.Nonempty String.Nonempty.NonemptyString)
    | SetAnswersResponse (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId) Int


type ToFrontend
    = CreateGameResponse (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) Evergreen.V9.User.User (Evergreen.V9.Id.Id Evergreen.V9.Id.GameId)
    | HostDataResponse (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) (AssocList.Dict (Evergreen.V9.Id.Id Evergreen.V9.Id.UserId) Evergreen.V9.User.User) (Maybe GameState)
    | LoadedGameToFrontend LoadedGameToFrontend
"""


oldUserModule =
    """module Evergreen.V5.User exposing (..)

import Evergreen.V5.ColorIndex


type alias User =
    { name : String
    , color : Evergreen.V5.ColorIndex.ColorIndex
    }
"""


userModule =
    """module Evergreen.V9.User exposing (..)

import Evergreen.V9.ColorIndex


type alias User =
    { name : String
    , color : Evergreen.V9.ColorIndex.ColorIndex
    }
"""
