module Internal.ExistingImport exposing (ExistingImport, defaults, isExposed)

import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range


type alias ExistingImport =
    { moduleName : ModuleName, moduleAlias : Maybe String, exposingList : Exposing }


isExposed : Exposing -> String -> Bool
isExposed exposing_ functionOrValue =
    case exposing_ of
        Elm.Syntax.Exposing.All _ ->
            True

        Elm.Syntax.Exposing.Explicit exposings ->
            List.any
                (\(Node _ a) ->
                    case a of
                        Elm.Syntax.Exposing.InfixExpose _ ->
                            False

                        Elm.Syntax.Exposing.FunctionExpose function ->
                            functionOrValue == function

                        Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAlias ->
                            functionOrValue == typeOrAlias

                        Elm.Syntax.Exposing.TypeExpose typeExpose ->
                            functionOrValue == typeExpose.name
                )
                exposings


node =
    Node Elm.Syntax.Range.emptyRange


defaults : List ExistingImport
defaults =
    [ { moduleName = [ "Basics" ], moduleAlias = Nothing, exposingList = All Elm.Syntax.Range.emptyRange }
    , { moduleName = [ "List" ], moduleAlias = Nothing, exposingList = Explicit [ node (TypeOrAliasExpose "List"), node (InfixExpose "::") ] }
    , { moduleName = [ "Maybe" ], moduleAlias = Nothing, exposingList = Explicit [ node (TypeExpose { name = "Maybe", open = Just Elm.Syntax.Range.emptyRange }) ] }
    , { moduleName = [ "Result" ], moduleAlias = Nothing, exposingList = Explicit [ node (TypeExpose { name = "Result", open = Just Elm.Syntax.Range.emptyRange }) ] }
    , { moduleName = [ "String" ], moduleAlias = Nothing, exposingList = Explicit [ node (TypeOrAliasExpose "String") ] }
    , { moduleName = [ "String" ], moduleAlias = Nothing, exposingList = Explicit [ node (TypeOrAliasExpose "Char") ] }
    , { moduleName = [ "Tuple" ], moduleAlias = Nothing, exposingList = Explicit [] }
    , { moduleName = [ "Debug" ], moduleAlias = Nothing, exposingList = Explicit [] }
    , { moduleName = [ "Platform" ], moduleAlias = Nothing, exposingList = Explicit [ node (TypeOrAliasExpose "Program") ] }
    , { moduleName = [ "Platform", "Cmd" ], moduleAlias = Just "Cmd", exposingList = Explicit [ node (TypeOrAliasExpose "Cmd") ] }
    , { moduleName = [ "Platform", "Sub" ], moduleAlias = Just "Sub", exposingList = Explicit [ node (TypeOrAliasExpose "Sub") ] }
    ]
