module Internal.Imports exposing (ExistingImport, defaults, importsFix)

import AssocSet as Set exposing (Set)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range
import Internal.Helpers exposing (node)
import Review.Fix


type alias ExistingImport =
    { moduleName : ModuleName, moduleAlias : Maybe String, exposingList : Exposing }


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


importsFix : ModuleName -> List ExistingImport -> Int -> Set ModuleName -> Maybe Review.Fix.Fix
importsFix currentModule existingImports importStartRow imports =
    let
        imports_ =
            Set.remove currentModule imports
                |> Set.filter
                    (\import_ -> List.any (.moduleName >> (==) import_) existingImports |> not)
    in
    if Set.isEmpty imports_ then
        Nothing

    else
        Set.toList imports_
            |> List.sort
            |> List.foldl
                (\import_ state -> state ++ "import " ++ String.join "." import_ ++ "\n")
                ""
            |> (\a -> a ++ "\n")
            |> Review.Fix.insertAt { row = importStartRow, column = 1 }
            |> Just
