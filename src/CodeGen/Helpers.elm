module CodeGen.Helpers exposing (application, capitalize, errorMessage, find, functionOrValue, getTypesHelper, hasDebugTodo, importsFix, node, notSupportedErrorMessage, parenthesis, parenthesisIfNecessary, pipeRight, typeAnnotationReturnValue, uncapitalize, varFromInt, writeDeclaration, debugLog)

import AssocSet as Set exposing (Set)
import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Pretty
import QualifiedType exposing (ExistingImport, QualifiedType, TypeAnnotation_(..), TypeOrTypeAlias(..))
import Review.Fix


writeDeclaration : Function -> String
writeDeclaration =
    Elm.Pretty.prettyFun >> Pretty.pretty 120


notSupportedErrorMessage : Node Expression
notSupportedErrorMessage =
    errorMessage "Can't handle this"


errorMessage : String -> Node Expression
errorMessage error =
    application
        [ functionOrValue [ "Debug" ] "todo"
        , Expression.Literal error |> node
        ]
        |> parenthesis


node =
    Node Elm.Syntax.Range.emptyRange


application =
    Expression.Application >> node


functionOrValue : ModuleName -> String -> Node Expression
functionOrValue moduleName functionOrValueName =
    Expression.FunctionOrValue moduleName functionOrValueName |> node


pipeRight : Node Expression -> Node Expression -> Node Expression
pipeRight eRight eLeft =
    Expression.OperatorApplication "|>" Infix.Left eLeft eRight |> node


parenthesis : Node Expression -> Node Expression
parenthesis =
    Expression.ParenthesizedExpression >> node


parenthesisIfNecessary : Node Expression -> Node Expression
parenthesisIfNecessary expr =
    case expr of
        Node _ (Expression.Application args) ->
            if List.length args > 1 then
                parenthesis expr

            else
                expr

        _ ->
            expr


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.
find (\\num -> num > 5) [ 2, 4, 6, 8 ]
--> Just 6

Borrowed from elm-community/list-extra

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


uncapitalize : String -> String
uncapitalize text =
    String.toLower (String.left 1 text) ++ String.dropLeft 1 text


capitalize : String -> String
capitalize text =
    String.toUpper (String.left 1 text) ++ String.dropLeft 1 text


hasDebugTodo : { a | expression : Node Expression } -> Bool
hasDebugTodo declaration =
    case declaration.expression of
        Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
            True

        _ ->
            False


varFromInt : Int -> String
varFromInt =
    (+) (Char.toCode 'a')
        >> Char.fromCode
        >> String.fromChar


debugLog : (String -> Node Expression -> Node Expression) -> Node Expression -> Node Expression
debugLog logger expr =
    logger (Elm.Pretty.prettyExpression (Elm.Syntax.Node.value expr) |> Pretty.pretty 120) expr


typeAnnotationReturnValue : Node TypeAnnotation -> Node TypeAnnotation
typeAnnotationReturnValue typeAnnotation =
    case typeAnnotation of
        Node _ (TypeAnnotation.FunctionTypeAnnotation _ node_) ->
            typeAnnotationReturnValue node_

        _ ->
            typeAnnotation


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


getTypesFromTypeAnnotation :
    List ( ModuleName, TypeOrTypeAlias )
    -> ModuleName
    -> Set QualifiedType
    -> TypeAnnotation_
    -> Set QualifiedType
getTypesFromTypeAnnotation types typeModuleName collectedTypes typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeParameters ->
            let
                collectedTypes_ : Set QualifiedType
                collectedTypes_ =
                    if QualifiedType.isPrimitiveType qualifiedType then
                        collectedTypes

                    else if Set.member qualifiedType collectedTypes then
                        collectedTypes

                    else
                        getTypesHelper
                            types
                            qualifiedType
                            (Set.insert qualifiedType collectedTypes)
                            |> Set.union collectedTypes
            in
            List.foldl
                (\typeParameter collectedTypes__ ->
                    getTypesFromTypeAnnotation types typeModuleName collectedTypes__ typeParameter
                        |> Set.union collectedTypes__
                )
                collectedTypes_
                typeParameters

        Tupled_ tupleValues ->
            List.foldl
                (\tupleValue collectedTypes_ ->
                    getTypesFromTypeAnnotation types typeModuleName collectedTypes_ tupleValue
                        |> Set.union collectedTypes_
                )
                collectedTypes
                tupleValues

        Record_ fields ->
            List.foldl
                (\( _, field ) collectedTypes_ ->
                    getTypesFromTypeAnnotation types typeModuleName collectedTypes_ field
                        |> Set.union collectedTypes_
                )
                collectedTypes
                fields

        _ ->
            collectedTypes


getTypesHelper :
    List ( ModuleName, TypeOrTypeAlias )
    -> QualifiedType
    -> Set QualifiedType
    -> Set QualifiedType
getTypesHelper types typeDeclaration collectedTypes =
    case QualifiedType.getTypeData types typeDeclaration of
        Just ( typeModuleName, TypeAliasValue _ fields ) ->
            List.foldl
                (\( _, typeAnnotation ) collectedTypes_ ->
                    getTypesFromTypeAnnotation types typeModuleName collectedTypes_ typeAnnotation
                )
                collectedTypes
                fields

        Just ( typeModuleName, TypeValue customType ) ->
            List.foldl
                (\constructor collectedTypes_ ->
                    List.foldl
                        (\typeAnnotation collectedTypes__ ->
                            getTypesFromTypeAnnotation types typeModuleName collectedTypes__ typeAnnotation
                        )
                        collectedTypes_
                        constructor.arguments
                )
                collectedTypes
                customType.constructors

        Nothing ->
            collectedTypes
