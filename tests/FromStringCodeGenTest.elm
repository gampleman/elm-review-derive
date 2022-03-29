module FromStringCodeGenTest exposing (..)

import CodeGenerator.Test exposing (codeGenTest)
import Test


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
        , -- Not sure this is worth supporting... How imprecise should this stuff be?
          Test.skip <| codeGenTest "custom type from string missing maybe" [] [] [ """module A exposing (..)

import Serialize exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> A a
fromString a = Debug.todo \"\"""" ] """module A exposing (..)

import Serialize exposing (Codec)

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
        , Test.skip <|
            codeGenTest "custom type from string missing parameter"
                []
                []
                [ """module A exposing (..)

import Serialize exposing (Codec)

type A a
    = A a
    | B Int String
    | C

fromString : String -> Maybe (A a)
fromString a =
    Debug.todo ""
""" ]
                """module A exposing (..)

import Serialize exposing (Codec)

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
        ]
