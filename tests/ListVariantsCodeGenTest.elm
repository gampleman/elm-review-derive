module ListVariantsCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (codeGenTest)
import Test


suite : Test.Test
suite =
    Test.describe "all variant"
        [ codeGenTest "list all variants" [] [] [ """module A exposing (..)

import Serialize exposing (Codec)

type A
    = A
    | B
    | C

list : List A
list =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type A
    = A
    | B
    | C

list : List A
list =
    [ A, B, C ]
"""
        ]
