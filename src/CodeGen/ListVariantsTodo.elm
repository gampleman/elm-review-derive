module CodeGen.ListVariantsTodo exposing (ListVariantsTodo, getListAllTodo, todoErrors)

import AssocList as Dict
import CodeGen.Helpers as Helpers
import Elm.Syntax.Expression as Expression exposing (Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import QualifiedType exposing (QualifiedType, TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule


type alias ListVariantsTodo =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    }


getListAllTodo :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Function
    -> Maybe ListVariantsTodo
getListAllTodo context declarationRange function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "List" )) [ Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    if Helpers.hasDebugTodo declaration then
                        case QualifiedType.create context.lookupTable context.currentModule codecType of
                            Just qualifiedType ->
                                { functionName = Node.value signature.name
                                , typeVar = qualifiedType
                                , range = declarationRange
                                }
                                    |> Just

                            Nothing ->
                                Nothing

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


generateListAllDefinition : ListVariantsTodo -> Type_ -> String
generateListAllDefinition toStringTodo type_ =
    { documentation = Nothing
    , signature =
        { name = Helpers.node toStringTodo.functionName
        , typeAnnotation =
            TypeAnnotation.Typed
                (Helpers.node ( [], "List" ))
                [ TypeAnnotation.Typed
                    (Helpers.node ( [], QualifiedType.name toStringTodo.typeVar ))
                    []
                    |> Helpers.node
                ]
                |> Helpers.node
        }
            |> Helpers.node
            |> Just
    , declaration =
        Helpers.node
            { name = Helpers.node toStringTodo.functionName
            , arguments = []
            , expression =
                List.map
                    (\constructor ->
                        Helpers.functionOrValue [] constructor.name
                            :: List.repeat
                                (List.length constructor.arguments)
                                Helpers.notSupportedErrorMessage
                            |> Helpers.application
                    )
                    type_.constructors
                    |> Expression.ListExpr
                    |> Helpers.node
            }
    }
        |> Helpers.writeDeclaration
        |> String.replace "," "\n    ,"


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
                    generateListAllDefinition todo type_
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
