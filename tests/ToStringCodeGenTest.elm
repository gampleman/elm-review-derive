module ToStringCodeGenTest exposing (..)

import Test
import TestHelper exposing (codeGenTest)


suite =
    Test.describe "toString"
        [ Test.skip <|
            codeGenTest "tree to string"
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
        , Test.skip <| codeGenTest "tree to string missing parameter" [] [ """module A exposing (..)

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
