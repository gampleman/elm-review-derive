module Internal.CodeGenerationResult exposing (CodeGenerationResult, appendBindingsAndDefsFrom, applyBindings, combine, combineMaybe, projectResult, succeed)

import Dict
import Elm.Syntax.Expression exposing (Expression, Function)
import Internal.Helpers as Helpers


type alias CodeGenerationResult b =
    Result
        -- Error msg
        String
        { expression : Expression

        -- Auxiliary definitions
        , auxiliaryDefinitions : List Function

        -- Bindings resulting from generics
        , bindings : List ( String, Expression )
        , computedValue : b
        }


combineHelp : List (CodeGenerationResult b) -> Result String { expressions : List Expression, auxiliaryDefinitions : List Function, bindings : List ( String, Expression ), computedValues : List b }
combineHelp =
    List.foldr
        (Result.map2
            (\item accum ->
                { expressions = item.expression :: accum.expressions
                , auxiliaryDefinitions = item.auxiliaryDefinitions ++ accum.auxiliaryDefinitions
                , bindings = item.bindings ++ accum.bindings
                , computedValues = item.computedValue :: accum.computedValues
                }
            )
        )
        (Ok
            { expressions = []
            , auxiliaryDefinitions = []
            , bindings = []
            , computedValues = []
            }
        )


combine : (List Expression -> Expression) -> (List b -> b) -> List (CodeGenerationResult b) -> CodeGenerationResult b
combine fn combiner list =
    combineHelp list
        |> Result.map
            (\accu ->
                { expression = fn accu.expressions
                , auxiliaryDefinitions = accu.auxiliaryDefinitions
                , bindings = accu.bindings
                , computedValue = combiner accu.computedValues
                }
            )


combineMaybe : (List Expression -> Maybe Expression) -> (List b -> b) -> List (CodeGenerationResult b) -> Maybe (CodeGenerationResult b)
combineMaybe fn combiner list =
    case combineHelp list of
        Ok accu ->
            Maybe.map
                (\expr ->
                    Ok
                        { expression = expr
                        , auxiliaryDefinitions = accu.auxiliaryDefinitions
                        , bindings = accu.bindings
                        , computedValue = combiner accu.computedValues
                        }
                )
                (fn accu.expressions)

        Err err ->
            Just (Err err)


appendBindingsAndDefsFrom : CodeGenerationResult b -> CodeGenerationResult b -> CodeGenerationResult b
appendBindingsAndDefsFrom =
    Result.map2
        (\source target ->
            { target
                | auxiliaryDefinitions = source.auxiliaryDefinitions ++ target.auxiliaryDefinitions
                , bindings = source.bindings ++ target.bindings
            }
        )


succeed : b -> Expression -> CodeGenerationResult b
succeed val expr =
    Ok
        { expression = expr
        , auxiliaryDefinitions = []
        , bindings = []
        , computedValue = val
        }


projectResult : CodeGenerationResult a -> Result String ( Expression, List Function )
projectResult =
    Result.map (\res -> ( res.expression, res.auxiliaryDefinitions ))


applyBindings : CodeGenerationResult a -> CodeGenerationResult a
applyBindings =
    Result.map
        (\res ->
            { res | expression = Helpers.applyBindings (Dict.fromList res.bindings) res.expression }
        )
