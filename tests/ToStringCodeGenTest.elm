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
        , Test.skip <|
            codeGenTest "tree to string"
                []
                []
                [ """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree a -> String
treeToString tree =
    Debug.todo ""
""" ]
                """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree -> String
treeToString tree =
    case tree of
        Node _ _ ->
            "Node"

        Leaf _ ->
            "Leaf\""""
        , Test.skip <| codeGenTest "tree to string missing parameter" [] [] [ """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree a -> String
treeToString =
    Debug.todo ""
""" ] """module A exposing (..)

type Tree a
    = Node (Tree a) (Tree a)
    | Leaf a

treeToString : Tree -> String
treeToString value =
    case value of
        Node _ _ ->
            "Node"

        Leaf _ ->
            "Leaf\""""
        ]
