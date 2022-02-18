module TestHelper exposing (codeGenTest)

import CodeGen
import Elm.CodeGen
import Elm.Parser
import Elm.Pretty
import Elm.Processing
import Elm.RawFile
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression
import Elm.Syntax.Node exposing (Node(..))
import Pretty
import Review.Test
import Test exposing (Test)


codeGenTest : String -> List String -> String -> Test
codeGenTest description modules expected =
    Test.test description <|
        \_ ->
            let
                result =
                    findTodo modules
            in
            Review.Test.runOnModules CodeGen.rule modules
                |> Review.Test.expectErrorsForModules
                    [ ( result.module_
                      , [ Review.Test.error
                            { message = "Here's my attempt to complete this stub"
                            , details = [ "" ]
                            , under = result.under
                            }
                            |> Review.Test.whenFixed expected
                        ]
                      )
                    ]


findTodo : List String -> { module_ : String, under : String }
findTodo modules =
    case modules of
        [] ->
            Debug.todo "You need to pass in a module that contains a Debug.todo"

        current :: rest ->
            case Elm.Parser.parse current of
                Ok rawFile ->
                    case
                        rawFile
                            |> Elm.Processing.process Elm.Processing.init
                            |> .declarations
                            |> List.filterMap
                                (\node ->
                                    case Elm.Syntax.Node.value node of
                                        FunctionDeclaration funDecl ->
                                            case (Elm.Syntax.Node.value funDecl.declaration).expression of
                                                Node _ (Elm.Syntax.Expression.Application ((Node _ (Elm.Syntax.Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
                                                    Just
                                                        { module_ = Elm.RawFile.moduleName rawFile |> String.join "."
                                                        , under = Elm.Syntax.Node.value node |> Elm.CodeGen.DeclNoComment |> Elm.Pretty.prettyDeclaration 4 |> Pretty.pretty 160
                                                        }

                                                _ ->
                                                    Nothing

                                        _ ->
                                            Nothing
                                )
                            |> List.head
                    of
                        Just res ->
                            res

                        Nothing ->
                            findTodo rest

                Err e ->
                    Debug.todo (Debug.toString e)
