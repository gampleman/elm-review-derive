module ListVariantsCodeGenTest exposing (..)

import Test
import TestHelper exposing (codeGenTest)


suite =
    Test.describe "all variant"
        [ codeGenTest "list all variants" [] [ """module A exposing (..)

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
