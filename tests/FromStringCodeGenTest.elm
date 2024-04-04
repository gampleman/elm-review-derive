module FromStringCodeGenTest exposing (suite)

import CodeGenerator.Test exposing (codeGenTest)
import Test


suite : Test.Test
suite =
    Test.describe "fromString"
        [ codeGenTest "custom type from string" [] [] [ """module A exposing (..)

import Serialize exposing (Codec)

type A
    = A
    | B Int String
    | C

fromString : String -> Maybe A
fromString a =
    Debug.todo ""
""" ] """module A exposing (..)

import Serialize exposing (Codec)

type A
    = A
    | B Int String
    | C

fromString : String -> Maybe A
fromString a =
    case a of
        "A" ->
            Just A

        "B" ->
            Just (B (Debug.todo "Can't handle this") (Debug.todo "Can't handle this"))

        "C" ->
            Just C

        _ ->
            Nothing
"""
        ]
