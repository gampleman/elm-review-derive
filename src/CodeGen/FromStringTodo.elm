module CodeGen.FromStringTodo exposing (FromStringTodo, getTodos, todoErrors)

import AssocList as Dict
import CodeGen.Helpers as Helpers
import Elm.Syntax.Expression as Expression exposing (Function)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import QualifiedType exposing (QualifiedType, TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule


type alias FromStringTodo =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    , parameterName : String
    }


getTodos :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Function
    -> Maybe FromStringTodo
getTodos context declarationRange function =
    let
        declaration : Expression.FunctionImplementation
        declaration =
            Node.value function.declaration

        createTodo : Signature -> Node ( ModuleName, String ) -> Maybe FromStringTodo
        createTodo signature customType =
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
                                , parameterName = "text"
                                }
                                    |> Just

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing

            else
                Nothing
    in
    case function.signature of
        Just (Node _ signature) ->
            case Node.value signature.typeAnnotation of
                TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( [], "Maybe" )) [ Node _ (TypeAnnotation.Typed customType _) ])) ->
                    createTodo signature customType

                TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) [])) (Node _ (TypeAnnotation.Typed customType _)) ->
                    createTodo signature customType

                _ ->
                    Nothing

        Nothing ->
            Nothing


generateFromStringDefinition : FromStringTodo -> Type_ -> String
generateFromStringDefinition toStringTodo type_ =
    { documentation = Nothing
    , signature =
        { name = Helpers.node toStringTodo.functionName
        , typeAnnotation =
            TypeAnnotation.FunctionTypeAnnotation
                (TypeAnnotation.Typed (Helpers.node ( [], "String" )) [] |> Helpers.node)
                (TypeAnnotation.Typed
                    (Helpers.node ( [], "Maybe" ))
                    [ TypeAnnotation.Typed (Helpers.node ( [], QualifiedType.name toStringTodo.typeVar )) [] |> Helpers.node ]
                    |> Helpers.node
                )
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
                                ( StringPattern constructor.name |> Helpers.node
                                , if List.isEmpty constructor.arguments then
                                    Helpers.application
                                        [ Helpers.functionOrValue [] "Just"
                                        , Helpers.functionOrValue [] constructor.name
                                        ]

                                  else
                                    Expression.OperatorApplication "|>"
                                        Right
                                        (Helpers.functionOrValue [] constructor.name
                                            :: List.repeat (List.length constructor.arguments) Helpers.notSupportedErrorMessage
                                            |> Helpers.application
                                        )
                                        (Helpers.functionOrValue [] "Just")
                                        |> Helpers.node
                                )
                            )
                            type_.constructors
                            ++ [ ( Helpers.node AllPattern, Helpers.functionOrValue [] "Nothing" ) ]
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
                    generateFromStringDefinition todo type_
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
