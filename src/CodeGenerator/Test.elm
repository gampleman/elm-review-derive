module CodeGenerator.Test exposing (codeGenTest, codeGenTestFailsWith, fakeDependency)

import Array
import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen
import Elm.Parser
import Elm.Pretty
import Elm.Processing
import Elm.Project
import Elm.RawFile
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Writer
import Expect exposing (Expectation)
import Json.Decode
import NoDebug.TodoOrToString
import Pretty
import Review.Project
import Review.Project.Dependency exposing (Dependency)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test)


codeGenTestHelper : String -> List Dependency -> List CodeGenerator -> List String -> ({ module_ : String, under : String } -> Review.Test.ReviewResult -> Expectation) -> Test
codeGenTestHelper description dependencies codeGens modules fn =
    Test.test description <|
        \_ ->
            let
                inputModules =
                    List.map (String.replace "\u{000D}" "") modules

                project =
                    List.foldl Review.Project.addDependency Review.Test.Dependencies.projectWithElmCore dependencies
            in
            case findTodo inputModules of
                Ok result ->
                    fn result (Review.Test.runOnModulesWithProjectData project (NoDebug.TodoOrToString.rule codeGens) inputModules)

                Err msg ->
                    Expect.fail msg


codeGenTestFailsWith : String -> List Dependency -> List CodeGenerator -> List String -> String -> Test
codeGenTestFailsWith description dependencies codeGens modules expectedFailureDetails =
    codeGenTestHelper description
        dependencies
        codeGens
        modules
        (\result ->
            Review.Test.expectErrorsForModules
                [ ( result.module_
                  , [ Review.Test.error
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production.", expectedFailureDetails ]
                        , under = result.under
                        }
                    ]
                  )
                ]
        )


codeGenTest : String -> List Dependency -> List CodeGenerator -> List String -> String -> Test
codeGenTest description dependencies codeGens modules expected =
    codeGenTestHelper description
        dependencies
        codeGens
        modules
        (\result ->
            Review.Test.expectErrorsForModules
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
        )


extractSubstring : Range -> String -> String
extractSubstring { start, end } file =
    case
        String.lines file
            |> Array.fromList
            |> Array.slice (start.row - 1) end.row
            |> Array.toList
    of
        [] ->
            ""

        fst :: rest ->
            case List.reverse (String.dropLeft (start.column - 1) fst :: rest) of
                [] ->
                    ""

                last :: head ->
                    (String.dropRight (String.length last - end.column) last :: head)
                        |> List.reverse
                        |> String.join "\n"


fakeDependency : String -> Dependency
fakeDependency name =
    case Json.Decode.decodeString Elm.Project.decoder ("""{
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
            Review.Project.Dependency.create name val []

        Err e ->
            Review.Test.Dependencies.elmCore


findTodo : List String -> Result String { module_ : String, under : String }
findTodo modules =
    case modules of
        [] ->
            Err "You need to pass in a module that actually contains a Debug.todo as a top-level value."

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
                                                        , under = extractSubstring (Elm.Syntax.Node.range node) current
                                                        }

                                                _ ->
                                                    Nothing

                                        _ ->
                                            Nothing
                                )
                            |> List.head
                    of
                        Just res ->
                            Ok res

                        Nothing ->
                            findTodo rest

                Err _ ->
                    Err ("Failed to parse the following file:\n\n```" ++ current ++ "\n```")
