module TestHelper exposing (codeGenTest, codeGenTestFailsWith, fakeDependency)

import Elm.CodeGen
import Elm.Parser
import Elm.Pretty
import Elm.Processing
import Elm.Project
import Elm.RawFile
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression
import Elm.Syntax.Node exposing (Node(..))
import Json.Decode
import NoDebug.TodoOrToString
import Pretty
import Review.Project
import Review.Project.Dependency exposing (Dependency)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test)


codeGenTestFailsWith : String -> List Dependency -> List String -> String -> Test
codeGenTestFailsWith description dependencies modules expectedFailureDetails =
    Test.test description <|
        \_ ->
            let
                inputModules =
                    List.map (String.replace "\u{000D}" "") modules

                result =
                    findTodo inputModules

                project =
                    List.foldl Review.Project.addDependency Review.Test.Dependencies.projectWithElmCore dependencies
            in
            Review.Test.runOnModulesWithProjectData project NoDebug.TodoOrToString.rule inputModules
                |> Review.Test.expectErrorsForModules
                    [ ( result.module_
                      , [ Review.Test.error
                            { message = "Remove the use of `Debug.todo` before shipping to production"
                            , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production.", expectedFailureDetails ]
                            , under = result.under
                            }
                        ]
                      )
                    ]


codeGenTest : String -> List Dependency -> List String -> String -> Test
codeGenTest description dependencies modules expected =
    Test.test description <|
        \_ ->
            let
                inputModules =
                    List.map (String.replace "\u{000D}" "") modules

                result =
                    findTodo inputModules

                project =
                    List.foldl Review.Project.addDependency Review.Test.Dependencies.projectWithElmCore dependencies
            in
            Review.Test.runOnModulesWithProjectData project NoDebug.TodoOrToString.rule inputModules
                |> Review.Test.expectErrorsForModules
                    [ ( result.module_
                      , [ Review.Test.error
                            { message = "Remove the use of `Debug.todo` before shipping to production"
                            , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                            , under = result.under
                            }
                            |> Review.Test.whenFixed (String.replace "\u{000D}" "" expected)
                        ]
                      )
                    ]


fakeDependency : String -> Dependency
fakeDependency name =
    Review.Project.Dependency.create name
        (case Json.Decode.decodeString Elm.Project.decoder ("""{
    "type": "package",
    "name": """ ++ "\"" ++ name ++ "\"" ++ """,
    "summary": "Extra functions for the core Random library",
    "license": "BSD-3-Clause",
    "version": "3.2.0",
    "exposed-modules": [],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}""") of
            Ok val ->
                val

            Err e ->
                Debug.todo (Debug.toString e)
        )
        []


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
