module CodeGen.CodecTodo exposing (CodecTodo, ProjectContext, codecTypeTodoFixes, declarationVisitorGetCodecs, getTodos, todoErrors)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import CodeGen.Helpers as Helpers
import Elm.CodeGen exposing (ModuleName)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import QualifiedType exposing (ExistingImport, QualifiedType, TypeAnnotation_(..), TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule exposing (Error, ModuleKey)


type alias CodecTodo =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    , parameters : List (Node Pattern)
    , signature : Signature
    }


declarationVisitorGetCodecs :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Function
    -> Maybe { functionName : String, typeVar : QualifiedType }
declarationVisitorGetCodecs context function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case Helpers.typeAnnotationReturnValue signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ _, Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    if Helpers.hasDebugTodo declaration then
                        Nothing

                    else
                        case QualifiedType.create context.lookupTable context.currentModule codecType of
                            Just qualifiedType ->
                                { functionName = Node.value signature.name
                                , typeVar = qualifiedType
                                }
                                    |> Just

                            Nothing ->
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getTodos :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Function
    -> Maybe CodecTodo
getTodos context declarationRange function =
    let
        declaration =
            Node.value function.declaration

        abc signature codecType =
            if Helpers.hasDebugTodo declaration then
                case QualifiedType.create context.lookupTable context.currentModule codecType of
                    Just qualifiedType ->
                        { functionName = Node.value signature.name
                        , typeVar = qualifiedType
                        , range = declarationRange
                        , parameters = declaration.arguments
                        , signature = signature
                        }
                            |> Just

                    Nothing ->
                        Nothing

            else
                Nothing
    in
    case function.signature of
        Just (Node _ signature) ->
            case Helpers.typeAnnotationReturnValue signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ _, Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    abc signature codecType

                Node _ (TypeAnnotation.Typed (Node _ ( [ "Serialize" ], "Codec" )) [ Node _ _, Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    abc signature codecType

                _ ->
                    Nothing

        _ ->
            Nothing


generateRecordCodec :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> Maybe QualifiedType
    -> List ( String, TypeAnnotation_ )
    -> ( Node Expression, Set ModuleName )
generateRecordCodec projectContext existingImports currentModule maybeTypeAlias recordFields =
    List.foldl
        (\( fieldName, typeAnnotation ) ( code, imports ) ->
            let
                ( codec, imports_ ) =
                    codecFromTypeAnnotation projectContext existingImports currentModule typeAnnotation
            in
            ( code
                |> Helpers.pipeRight
                    (Helpers.application
                        [ Helpers.functionOrValue [ "Codec" ] "field"
                        , Helpers.functionOrValue [ "" ] fieldName
                        , codec
                        ]
                    )
            , Set.union imports imports_
            )
        )
        ( Helpers.application
            [ Helpers.functionOrValue [ "Codec" ] "record"
            , case maybeTypeAlias of
                Just typeAliasName ->
                    Helpers.functionOrValue
                        (QualifiedType.moduleName currentModule existingImports typeAliasName)
                        (QualifiedType.name typeAliasName)

                Nothing ->
                    Expression.LambdaExpression
                        { args =
                            List.range 0 (List.length recordFields - 1)
                                |> List.map (Helpers.varFromInt >> VarPattern >> Helpers.node)
                        , expression =
                            List.indexedMap
                                (\index ( fieldName, _ ) ->
                                    ( Helpers.node fieldName, Helpers.functionOrValue [] (Helpers.varFromInt index) ) |> Helpers.node
                                )
                                recordFields
                                |> Expression.RecordExpr
                                |> Helpers.node
                        }
                        |> Helpers.node
                        |> Helpers.parenthesis
            ]
        , case maybeTypeAlias of
            Just typeAliasName ->
                QualifiedType.qualifiedPath typeAliasName |> Set.singleton

            Nothing ->
                Set.empty
        )
        recordFields
        |> Tuple.mapFirst (Helpers.pipeRight (Helpers.application [ Helpers.functionOrValue [ "Codec" ] "finishRecord" ]))


generateCodecTodoDefinition : ProjectContext a -> ModuleName -> List ExistingImport -> CodecTodo -> TypeOrTypeAlias -> ( String, Set ModuleName )
generateCodecTodoDefinition projectContext currentModule existingImports codecTodo typeOrTypeAlias =
    let
        ( expression, imports ) =
            case typeOrTypeAlias of
                TypeValue typeValue ->
                    generateCustomTypeCodec
                        projectContext
                        currentModule
                        existingImports
                        typeValue

                TypeAliasValue typeAliasName fields ->
                    generateRecordCodec
                        projectContext
                        existingImports
                        currentModule
                        (Just typeAliasName)
                        fields
    in
    ( { documentation = Nothing
      , signature = Helpers.node codecTodo.signature |> Just
      , declaration =
            Helpers.node
                { name = Helpers.node codecTodo.functionName
                , arguments = codecTodo.parameters
                , expression = expression
                }
      }
        |> Helpers.writeDeclaration
    , imports
    )


type alias ProjectContext a =
    { a
        | types : List ( ModuleName, TypeOrTypeAlias )
        , codecs : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
        , codecTodos : List ( ModuleName, CodecTodo )
        , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
        , moduleKeys : Dict ModuleName ModuleKey
    }


codecFromTypeAnnotation :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> TypeAnnotation_
    -> ( Node Expression, Set ModuleName )
codecFromTypeAnnotation projectContext existingImports currentModule typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeVariables ->
            let
                getCodecName : String -> Node Expression
                getCodecName text =
                    case text of
                        "Int" ->
                            Helpers.functionOrValue [ "Codec" ] "int"

                        "Float" ->
                            Helpers.functionOrValue [ "Codec" ] "float"

                        "String" ->
                            Helpers.functionOrValue [ "Codec" ] "string"

                        "Bool" ->
                            Helpers.functionOrValue [ "Codec" ] "bool"

                        "Maybe" ->
                            Helpers.functionOrValue [ "Codec" ] "maybe"

                        "Dict" ->
                            Helpers.functionOrValue [ "Codec" ] "dict"

                        "Set" ->
                            Helpers.functionOrValue [ "Codec" ] "set"

                        "Result" ->
                            Helpers.functionOrValue [ "Codec" ] "result"

                        "List" ->
                            Helpers.functionOrValue [ "Codec" ] "list"

                        "Array" ->
                            Helpers.functionOrValue [ "Codec" ] "array"

                        _ ->
                            Helpers.functionOrValue [] (Helpers.uncapitalize text ++ "Codec")

                applied : ( Node Expression, Set ModuleName )
                applied =
                    ( case Helpers.find (.typeVar >> (==) qualifiedType) projectContext.codecs of
                        Just codec ->
                            Helpers.functionOrValue codec.moduleName codec.functionName

                        Nothing ->
                            getCodecName (QualifiedType.name qualifiedType)
                    , Set.empty
                    )
                        :: List.map (codecFromTypeAnnotation projectContext existingImports currentModule) typeVariables
                        |> (\list ->
                                ( List.map Tuple.first list |> Helpers.application
                                , List.foldl (\( _, value ) state -> Set.union value state) Set.empty list
                                )
                           )
            in
            if List.isEmpty typeVariables then
                applied

            else
                Tuple.mapFirst Helpers.parenthesis applied

        Unit_ ->
            ( Helpers.functionOrValue [ "Codec" ] "unit", Set.empty )

        Tupled_ [ first ] ->
            codecFromTypeAnnotation projectContext existingImports currentModule first

        Tupled_ [ first, second ] ->
            let
                ( codec0, imports0 ) =
                    codecFromTypeAnnotation projectContext existingImports currentModule first

                ( codec1, imports1 ) =
                    codecFromTypeAnnotation projectContext existingImports currentModule second
            in
            ( Helpers.application
                [ Helpers.functionOrValue [ "Codec" ] "tuple"
                , codec0
                , codec1
                ]
                |> Helpers.parenthesis
            , Set.union imports0 imports1
            )

        Tupled_ [ first, second, third ] ->
            let
                ( codec0, imports0 ) =
                    codecFromTypeAnnotation projectContext existingImports currentModule first

                ( codec1, imports1 ) =
                    codecFromTypeAnnotation projectContext existingImports currentModule second

                ( codec2, imports2 ) =
                    codecFromTypeAnnotation projectContext existingImports currentModule third
            in
            ( Helpers.application
                [ Helpers.functionOrValue [ "Codec" ] "triple"
                , codec0
                , codec1
                , codec2
                ]
                |> Helpers.parenthesis
            , Set.union imports0 imports1 |> Set.union imports2
            )

        Tupled_ _ ->
            ( Helpers.functionOrValue [ "Codec" ] "unit", Set.empty )

        FunctionTypeAnnotation_ _ _ ->
            ( Helpers.errorMessage "Functions can't be serialized", Set.empty )

        GenericType_ _ ->
            ( Helpers.notSupportedErrorMessage, Set.empty )

        Record_ fields ->
            generateRecordCodec projectContext existingImports currentModule Nothing fields |> Tuple.mapFirst Helpers.parenthesis

        GenericRecord_ _ _ ->
            ( Helpers.notSupportedErrorMessage, Set.empty )


generateCustomTypeCodec : ProjectContext a -> ModuleName -> List ExistingImport -> Type_ -> ( Node Expression, Set ModuleName )
generateCustomTypeCodec projectContext currentModule existingImports customType =
    let
        moduleName_ =
            QualifiedType.moduleName currentModule existingImports customType.qualifiedType

        args : List ( Node Pattern, ( Node Pattern, Node Expression ) )
        args =
            customType.constructors
                |> List.indexedMap
                    (\index constructor ->
                        let
                            var =
                                Helpers.varFromInt index

                            arguments =
                                List.range 0 (List.length constructor.arguments - 1)
                                    |> List.map (String.fromInt >> (++) "data")
                        in
                        ( VarPattern var |> Helpers.node
                        , ( NamedPattern
                                { moduleName = moduleName_, name = constructor.name }
                                (List.map (VarPattern >> Helpers.node) arguments)
                                |> Helpers.node
                          , Helpers.application (Helpers.functionOrValue [] var :: List.map (Helpers.functionOrValue []) arguments)
                          )
                        )
                    )

        start =
            ( Helpers.application
                [ Helpers.functionOrValue [ "Codec" ] "customType"
                , Expression.LambdaExpression
                    { args = List.map Tuple.first args ++ [ VarPattern "value" |> Helpers.node ]
                    , expression =
                        Expression.CaseExpression
                            { expression = Helpers.functionOrValue [] "value", cases = List.map Tuple.second args }
                            |> Helpers.node
                    }
                    |> Helpers.node
                    |> Helpers.parenthesis
                ]
            , Set.singleton (QualifiedType.qualifiedPath customType.qualifiedType)
            )
    in
    List.foldl
        (\constructor ( code, imports ) ->
            let
                ( argumentsCode, imports_ ) =
                    List.map
                        (codecFromTypeAnnotation projectContext existingImports currentModule)
                        constructor.arguments
                        |> (\list ->
                                ( List.map Tuple.first list
                                , List.foldl (\( _, a ) b -> Set.union a b) Set.empty list
                                )
                           )
            in
            ( Helpers.pipeRight
                (Helpers.application
                    (Helpers.functionOrValue [ "Codec" ]
                        ("variant" ++ String.fromInt (List.length constructor.arguments))
                        :: Helpers.functionOrValue moduleName_ constructor.name
                        :: argumentsCode
                    )
                )
                code
            , Set.union imports imports_
            )
        )
        start
        customType.constructors
        |> (\( expression, imports ) ->
                ( Helpers.pipeRight
                    (Helpers.functionOrValue [ "Codec" ] "finishCustomType")
                    expression
                , imports
                )
           )


todoErrors :
    ProjectContext a
    -> ModuleName
    -> List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
    -> CodecTodo
    -> Maybe (Error { useErrorForModule : () })
todoErrors projectContext currentModule typeTodoFixes codecTodo =
    let
        maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
        maybeType =
            QualifiedType.getTypeData projectContext.types codecTodo.typeVar
    in
    case
        ( maybeType
        , Dict.get currentModule projectContext.moduleKeys
        , Dict.get currentModule projectContext.imports
        )
    of
        ( Just ( _, type_ ), Just moduleKey, Just { newImportStartRow, existingImports } ) ->
            let
                ( fix, imports0 ) =
                    generateCodecTodoDefinition projectContext currentModule existingImports codecTodo type_

                ( fixes, imports1 ) =
                    List.filterMap
                        (\data ->
                            if data.moduleName == currentModule then
                                Just ( data.fix, data.newImports )

                            else
                                Nothing
                        )
                        typeTodoFixes
                        |> (\list ->
                                ( List.map Tuple.first list
                                , List.foldl
                                    (\( _, imports_ ) state -> Set.union imports_ state)
                                    Set.empty
                                    list
                                )
                           )
            in
            Rule.errorForModuleWithFix
                moduleKey
                { message = "Here's my attempt to complete this stub"
                , details = [ "" ]
                }
                codecTodo.range
                (Review.Fix.replaceRangeBy codecTodo.range fix
                    :: (case
                            Helpers.importsFix
                                currentModule
                                existingImports
                                newImportStartRow
                                (Set.union imports0 imports1)
                        of
                            Just importsFix_ ->
                                [ importsFix_ ]

                            Nothing ->
                                []
                       )
                    ++ fixes
                )
                |> Just

        _ ->
            Nothing


getCodecTypes : ProjectContext a -> Dict ModuleName (Set QualifiedType)
getCodecTypes projectContext =
    List.foldl
        (\( moduleName, todo ) dict ->
            Dict.update
                moduleName
                (Maybe.withDefault Set.empty
                    >> Helpers.getTypesHelper projectContext.types todo.typeVar
                    >> Just
                )
                dict
        )
        Dict.empty
        projectContext.codecTodos


codecTypeTodoFixes : ProjectContext a -> List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
codecTypeTodoFixes projectContext =
    getCodecTypes projectContext
        |> Dict.toList
        |> List.concatMap
            (\( currentModule, qualifiedTypes ) ->
                let
                    todosInModule : List CodecTodo
                    todosInModule =
                        List.filterMap
                            (\( moduleName_, todo ) ->
                                if moduleName_ == currentModule then
                                    Just todo

                                else
                                    Nothing
                            )
                            projectContext.codecTodos
                in
                Set.toList qualifiedTypes
                    |> List.filter (\a -> List.any (.typeVar >> (/=) a) todosInModule)
                    |> List.filter (\a -> List.all (.typeVar >> (/=) a) projectContext.codecs)
                    |> List.indexedMap
                        (\index qualifiedType ->
                            let
                                maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                maybeType =
                                    QualifiedType.getTypeData projectContext.types qualifiedType
                            in
                            case ( maybeType, Dict.get currentModule projectContext.imports ) of
                                ( Just ( _, type_ ), Just { existingImports } ) ->
                                    let
                                        name =
                                            Helpers.uncapitalize (QualifiedType.name qualifiedType) ++ "Codec"

                                        position =
                                            { column = 0, row = 99999 + index }

                                        todo =
                                            { functionName = name
                                            , typeVar = qualifiedType
                                            , range =
                                                { start = position
                                                , end = position
                                                }
                                            , signature =
                                                { name = Helpers.node name
                                                , typeAnnotation =
                                                    TypeAnnotation.Typed
                                                        (Helpers.node ( [], "Codec" ))
                                                        [ TypeAnnotation.GenericType "e" |> Helpers.node
                                                        , QualifiedType.toString currentModule existingImports qualifiedType
                                                            |> TypeAnnotation.GenericType
                                                            |> Helpers.node
                                                        ]
                                                        |> Helpers.node
                                                }
                                            , parameters = []
                                            }

                                        ( fix, newImports ) =
                                            generateCodecTodoDefinition
                                                projectContext
                                                currentModule
                                                existingImports
                                                todo
                                                type_
                                    in
                                    { moduleName = currentModule
                                    , fix = Review.Fix.insertAt position fix
                                    , newImports = Set.insert (QualifiedType.qualifiedPath qualifiedType) newImports
                                    }
                                        |> Just

                                _ ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
