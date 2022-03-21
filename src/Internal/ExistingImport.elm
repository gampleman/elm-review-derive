module Internal.ExistingImport exposing (..)

import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))


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
