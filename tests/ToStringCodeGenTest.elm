module ToStringCodeGenTest exposing (..)

import CodeGenerator.Test exposing (codeGenTest)
import Test


suite =
    Test.describe "toString"
        [ codeGenTest "Simple custom type to string"
            []
            []
            [ """module A exposing (..)

type Foo 
    = Foo | Bar

toString : Foo a -> String
toString foo =
    Debug.todo ""
""" ]
            """module A exposing (..)

type Foo 
    = Foo | Bar

toString : Foo a -> String
toString foo =
    case foo of
        Foo ->
            "Foo"

        Bar ->
            "Bar"
"""
        ]
