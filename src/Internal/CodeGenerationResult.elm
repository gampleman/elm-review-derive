module Internal.CodeGenerationResult exposing (CodeGenerationResult, applyBindings, combine, combineMaybe, projectResult, succeed)

import Dict
import Elm.Syntax.Expression exposing (Expression, Function)
import Internal.Helpers as Helpers


type alias CodeGenerationResult =
    Result
        -- Error msg
        String
        { expression : Expression

        -- Auxiliary definitions
        , auxiliaryDefinitions : List Function

        -- Bindings resulting from generics
        , bindings : List ( String, Expression )
        }


combineHelp : List CodeGenerationResult -> Result String { expressions : List Expression, auxiliaryDefinitions : List Function, bindings : List ( String, Expression ) }
combineHelp =
    List.foldr
        (Result.map2
            (\item accum ->
                { expressions = item.expression :: accum.expressions
                , auxiliaryDefinitions = item.auxiliaryDefinitions ++ accum.auxiliaryDefinitions
                , bindings = item.bindings ++ accum.bindings
                }
            )
        )
        (Ok
            { expressions = []
            , auxiliaryDefinitions = []
            , bindings = []
            }
        )


combine : (List Expression -> Expression) -> List CodeGenerationResult -> CodeGenerationResult
combine fn list =
    combineHelp list
        |> Result.map
            (\accu ->
                { expression = fn accu.expressions
                , auxiliaryDefinitions = accu.auxiliaryDefinitions
                , bindings = accu.bindings
                }
            )


combineMaybe : (List Expression -> Maybe Expression) -> List CodeGenerationResult -> Maybe CodeGenerationResult
combineMaybe fn list =
    case combineHelp list of
        Ok accu ->
            Maybe.map
                (\expr ->
                    Ok
                        { expression = expr
                        , auxiliaryDefinitions = accu.auxiliaryDefinitions
                        , bindings = accu.bindings
                        }
                )
                (fn accu.expressions)

        Err err ->
            Just (Err err)


succeed : Expression -> CodeGenerationResult
succeed expr =
    Ok
        { expression = expr
        , auxiliaryDefinitions = []
        , bindings = []
        }


projectResult : CodeGenerationResult -> Result String ( Expression, List Function )
projectResult =
    Result.map (\res -> ( res.expression, res.auxiliaryDefinitions ))


applyBindings : CodeGenerationResult -> CodeGenerationResult
applyBindings =
    Result.map
        (\res ->
            { res | expression = Helpers.applyBindings (Dict.fromList res.bindings) res.expression }
        )
