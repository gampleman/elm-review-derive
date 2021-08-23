module CodeGen.ToStringTodo exposing (ToStringTodo, getTodos, todoErrors)

import AssocList as Dict
import CodeGen.Helpers as Helpers
import Elm.Syntax.Expression as Expression exposing (Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import QualifiedType exposing (QualifiedType, TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule


type alias ToStringTodo =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    , parameterName : String
    }


getTodos :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Function
    -> Maybe ToStringTodo
getTodos context declarationRange function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case signature.typeAnnotation of
                Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed customType _)) (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) []))) ->
                    if Helpers.hasDebugTodo declaration then
                        case QualifiedType.create context.lookupTable context.currentModule customType of
                            Just qualifiedType ->
                                case declaration.arguments of
                                    (Node _ (VarPattern parameter)) :: [] ->
                                        { functionName = Node.value signature.name
                                        , typeVar = qualifiedType
                                        , range = declarationRange
                                        , parameterName = parameter
                                        }
                                            |> Just

                                    [] ->
                                        { functionName = Node.value signature.name
                                        , typeVar = qualifiedType
                                        , range = declarationRange
                                        , parameterName = "value"
                                        }
                                            |> Just

                                    _ ->
                                        Nothing

                            Nothing ->
                                Nothing

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


generateToStringDefinition : ToStringTodo -> Type_ -> String
generateToStringDefinition toStringTodo type_ =
    { documentation = Nothing
    , signature =
        { name = Helpers.node toStringTodo.functionName
        , typeAnnotation =
            TypeAnnotation.FunctionTypeAnnotation
                (TypeAnnotation.Typed (Helpers.node ( [], QualifiedType.name toStringTodo.typeVar )) []
                    |> Helpers.node
                )
                (TypeAnnotation.Typed (Helpers.node ( [], "String" )) [] |> Helpers.node)
                |> Helpers.node
        }
            |> Helpers.node
            |> Just
    , declaration =
        Helpers.node
            { name = Helpers.node toStringTodo.functionName
            , arguments = [ VarPattern toStringTodo.parameterName |> Helpers.node ]
            , expression =
                Expression.CaseExpression
                    { expression = Helpers.functionOrValue [] toStringTodo.parameterName
                    , cases =
                        List.map
                            (\constructor ->
                                ( NamedPattern
                                    { moduleName = []
                                    , name = constructor.name
                                    }
                                    (List.repeat (List.length constructor.arguments) (Helpers.node AllPattern))
                                    |> Helpers.node
                                , Expression.Literal constructor.name |> Helpers.node
                                )
                            )
                            type_.constructors
                    }
                    |> Helpers.node
            }
    }
        |> Helpers.writeDeclaration


todoErrors projectContext moduleName todo =
    let
        maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
        maybeType =
            QualifiedType.getTypeData projectContext.types todo.typeVar
    in
    case ( maybeType, Dict.get moduleName projectContext.moduleKeys ) of
        ( Just ( _, TypeValue type_ ), Just moduleKey ) ->
            let
                fix =
                    generateToStringDefinition todo type_
            in
            Rule.errorForModuleWithFix
                moduleKey
                { message = "Here's my attempt to complete this stub"
                , details = [ "" ]
                }
                todo.range
                [ Review.Fix.replaceRangeBy todo.range fix ]
                |> Just

        _ ->
            Nothing
