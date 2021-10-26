module CodeGen.MigrateTodo exposing (MigrateTodo, declarationVisitorGetMigrateFunctions, getTodos, migrateTypeTodoFixes, todoErrors)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import CodeGen.Helpers as Helpers
import Dict as RegularDict
import Elm.Syntax.Expression as Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import List.Extra as List
import QualifiedType exposing (ExistingImport, QualifiedType, TypeAnnotation_(..), TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule exposing (ModuleKey)


type alias ProjectContext a =
    { a
        | types : List ( ModuleName, TypeOrTypeAlias )
        , migrateFunctions :
            List
                { moduleName : ModuleName
                , functionName : String
                , oldType : QualifiedType
                , newType : QualifiedType
                }
        , migrateTodos : List ( ModuleName, MigrateTodo )
        , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
        , moduleKeys : Dict ModuleName ModuleKey
    }


type alias MigrateTodo =
    { functionName : String
    , oldType : QualifiedType
    , newType : QualifiedType
    , range : Range
    , signature : Signature
    }


getTodos :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Function
    -> Maybe MigrateTodo
getTodos context declarationRange function =
    let
        declaration : Expression.FunctionImplementation
        declaration =
            Node.value function.declaration
    in
    case function.signature of
        Just (Node _ signature) ->
            case Node.value signature.typeAnnotation of
                FunctionTypeAnnotation (Node _ (Typed oldType [])) (Node _ (Typed newType _)) ->
                    createTodo context declarationRange declaration signature oldType newType

                _ ->
                    Nothing

        Nothing ->
            Nothing


createTodo :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Expression.FunctionImplementation
    -> Signature
    -> Node ( ModuleName, String )
    -> Node ( ModuleName, String )
    -> Maybe MigrateTodo
createTodo context declarationRange declaration signature oldQualifiedType newQualifiedType =
    if Helpers.hasDebugTodo declaration then
        case
            ( QualifiedType.create context.lookupTable context.currentModule oldQualifiedType
            , QualifiedType.create context.lookupTable context.currentModule newQualifiedType
            )
        of
            ( Just oldType, Just newType ) ->
                { functionName = Node.value signature.name
                , oldType = oldType
                , newType = newType
                , range = declarationRange
                , signature = signature
                }
                    |> Just

            _ ->
                Nothing

    else
        Nothing


getCases : ModuleName -> List ExistingImport -> Type_ -> Type_ -> List ( Node Pattern, Node Expression )
getCases currentModule existingImports oldTypeValue newTypeValue =
    let
        oldActualImportName =
            QualifiedType.moduleName currentModule existingImports oldTypeValue.qualifiedType

        newActualImportName =
            QualifiedType.moduleName currentModule existingImports newTypeValue.qualifiedType
    in
    List.map
        (\oldConstructor ->
            let
                parameters : List (Node Pattern)
                parameters =
                    List.indexedMap
                        (\index _ ->
                            Helpers.varFromInt index |> VarPattern |> Helpers.node
                        )
                        oldConstructor.arguments
            in
            ( Helpers.node
                (NamedPattern { moduleName = oldActualImportName, name = oldConstructor.name } parameters)
            , case List.find (.name >> (==) oldConstructor.name) newTypeValue.constructors of
                Just newConstructor ->
                    if List.length oldConstructor.arguments == List.length newConstructor.arguments then
                        Helpers.application
                            (Helpers.functionOrValue newActualImportName newConstructor.name
                                :: List.indexedMap
                                    (\index argument ->
                                        parameterToExpression (Just index) (isMigratable argument argument)
                                    )
                                    newConstructor.arguments
                            )

                    else
                        Helpers.notSupportedErrorMessage

                Nothing ->
                    Helpers.notSupportedErrorMessage
            )
        )
        oldTypeValue.constructors


generateMigrationDefinition :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> MigrateTodo
    -> TypeOrTypeAlias
    -> TypeOrTypeAlias
    -> ( String, Set ModuleName )
generateMigrationDefinition projectContext existingImports currentModule migrateTodo oldType newType =
    let
        expression : Node Expression
        expression =
            case ( oldType, newType ) of
                ( TypeValue oldTypeValue, TypeValue newTypeValue ) ->
                    let
                        cases : List ( Node Pattern, Node Expression )
                        cases =
                            getCases currentModule existingImports oldTypeValue newTypeValue
                    in
                    CaseExpression
                        { expression = Helpers.functionOrValue [] "old"
                        , cases = cases
                        }
                        |> Helpers.node

                ( TypeAliasValue _ oldTypeAlias, TypeAliasValue _ newTypeAlias ) ->
                    migrateTypeAlias oldTypeAlias newTypeAlias

                _ ->
                    Helpers.notSupportedErrorMessage
    in
    ( { documentation = Nothing
      , signature = Just (Helpers.node migrateTodo.signature)
      , declaration =
            Helpers.node
                { name = Helpers.node migrateTodo.functionName
                , arguments = [ VarPattern "old" |> Helpers.node ]
                , expression = expression
                }
      }
        |> Helpers.writeDeclaration
    , Set.empty
    )


migrateTypeAlias : List ( String, TypeAnnotation_ ) -> List ( String, TypeAnnotation_ ) -> Node Expression
migrateTypeAlias oldTypeAlias newTypeAlias =
    if oldTypeAlias == newTypeAlias then
        Helpers.functionOrValue [] "old"

    else
        let
            oldFields : RegularDict.Dict String TypeAnnotation_
            oldFields =
                RegularDict.fromList oldTypeAlias

            newFields : RegularDict.Dict String TypeAnnotation_
            newFields =
                RegularDict.fromList newTypeAlias
        in
        if RegularDict.diff oldFields newFields |> RegularDict.isEmpty then
            List.map
                (\( fieldName, value ) ->
                    ( Helpers.node fieldName
                    , case RegularDict.get fieldName oldFields of
                        Just oldValue ->
                            if value == oldValue then
                                RecordAccess
                                    (Helpers.functionOrValue [] "old")
                                    (Helpers.node fieldName)
                                    |> Helpers.node

                            else
                                recordFieldToExpression fieldName (isMigratable oldValue value)

                        Nothing ->
                            Helpers.notSupportedErrorMessage
                    )
                        |> Helpers.node
                )
                newTypeAlias
                |> RecordExpr
                |> Helpers.node

        else
            Helpers.notSupportedErrorMessage


recordFieldToExpression : String -> Migratable -> Node Expression
recordFieldToExpression fieldName migratable =
    let
        recordAccess =
            RecordAccess
                (Helpers.functionOrValue [] "old")
                (Helpers.node fieldName)
                |> Helpers.node
    in
    case migratable of
        Migratable migrationFunctionName migrationTypeVars ->
            Helpers.application
                (Helpers.functionOrValue [] migrationFunctionName
                    :: List.map (parameterToExpression Nothing) migrationTypeVars
                    ++ [ recordAccess ]
                )

        IsPrimitive ->
            recordAccess

        NotMigratable ->
            Helpers.notSupportedErrorMessage

        MigrateRecord fields ->
            migrateRecord recordAccess fields


migrateRecord : Node Expression -> List ( String, Migratable ) -> Node Expression
migrateRecord oldValue fields =
    List.map
        (\( fieldName_, fieldValue ) ->
            ( Helpers.node fieldName_
            , Helpers.application
                [ parameterToExpression Nothing fieldValue
                , RecordAccess oldValue (Helpers.node fieldName_) |> Helpers.node
                ]
            )
                |> Helpers.node
        )
        fields
        |> RecordExpr
        |> Helpers.node


parameterToExpression : Maybe Int -> Migratable -> Node Expression
parameterToExpression maybeIndex migratable =
    case migratable of
        Migratable migrationFunctionName migrationTypeVars ->
            Helpers.application
                (Helpers.functionOrValue [] migrationFunctionName
                    :: List.map (parameterToExpression Nothing) migrationTypeVars
                    ++ (case maybeIndex of
                            Just index ->
                                [ Helpers.varFromInt index |> Helpers.functionOrValue [] ]

                            Nothing ->
                                []
                       )
                )
                |> (\a ->
                        if List.isEmpty migrationTypeVars && maybeIndex == Nothing then
                            a

                        else
                            Helpers.parenthesis a
                   )

        IsPrimitive ->
            case maybeIndex of
                Just index ->
                    Helpers.varFromInt index |> Helpers.functionOrValue []

                Nothing ->
                    Helpers.functionOrValue [] "identity"

        NotMigratable ->
            Helpers.notSupportedErrorMessage

        MigrateRecord fields ->
            case maybeIndex of
                Just index ->
                    migrateRecord
                        (Helpers.varFromInt index |> Helpers.functionOrValue [])
                        fields

                Nothing ->
                    --Debug.todo ""
                    Helpers.notSupportedErrorMessage


type Migratable
    = NotMigratable
    | IsPrimitive
    | Migratable String (List Migratable)
    | MigrateRecord (List ( String, Migratable ))


isMigratable : TypeAnnotation_ -> TypeAnnotation_ -> Migratable
isMigratable oldType newType =
    case ( oldType, newType ) of
        ( Typed_ oldQualifiedType oldTypeVars, Typed_ newQualifiedType newTypeVars ) ->
            let
                newName =
                    QualifiedType.name newQualifiedType
            in
            if List.length oldTypeVars == List.length newTypeVars then
                if QualifiedType.name oldQualifiedType == newName then
                    if isPrimitive newName then
                        IsPrimitive

                    else
                        Migratable
                            ("migrate" ++ Helpers.capitalize newName)
                            (List.map2
                                (\oldTypeVar newTypeVar ->
                                    isMigratable oldTypeVar newTypeVar
                                )
                                oldTypeVars
                                newTypeVars
                            )

                else
                    NotMigratable

            else
                NotMigratable

        ( Tupled_ [ oldFirst, oldSecond ], Tupled_ [ newFirst, newSecond ] ) ->
            Migratable "migrateTuple" [ isMigratable oldFirst newFirst, isMigratable oldSecond newSecond ]

        ( Tupled_ [ oldFirst, oldSecond, oldThird ], Tupled_ [ newFirst, newSecond, newThird ] ) ->
            Migratable
                "migrateTriple"
                [ isMigratable oldFirst newFirst, isMigratable oldSecond newSecond, isMigratable oldThird newThird ]

        ( Record_ oldTypeAlias, Record_ newTypeAlias ) ->
            let
                oldFields : RegularDict.Dict String TypeAnnotation_
                oldFields =
                    RegularDict.fromList oldTypeAlias

                newFields : RegularDict.Dict String TypeAnnotation_
                newFields =
                    RegularDict.fromList newTypeAlias
            in
            if RegularDict.diff oldFields newFields |> RegularDict.isEmpty then
                List.map
                    (\( fieldName, value ) ->
                        ( fieldName
                        , case RegularDict.get fieldName oldFields of
                            Just oldValue ->
                                isMigratable oldValue value

                            Nothing ->
                                NotMigratable
                        )
                    )
                    newTypeAlias
                    |> MigrateRecord

            else
                NotMigratable

        _ ->
            NotMigratable


isPrimitive : String -> Bool
isPrimitive name =
    List.any ((==) name) [ "Float", "Int", "String", "Bool" ]


declarationVisitorGetMigrateFunctions :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Function
    -> Maybe { functionName : String, oldType : QualifiedType, newType : QualifiedType }
declarationVisitorGetMigrateFunctions context function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case Node.value signature.typeAnnotation of
                FunctionTypeAnnotation (Node _ (Typed oldType [])) (Node _ (Typed newType _)) ->
                    if Helpers.hasDebugTodo declaration then
                        Nothing

                    else
                        case
                            ( QualifiedType.create context.lookupTable context.currentModule oldType
                            , QualifiedType.create context.lookupTable context.currentModule newType
                            )
                        of
                            ( Just oldQualifiedType, Just newQualifiedType ) ->
                                { functionName = Node.value signature.name
                                , oldType = oldQualifiedType
                                , newType = newQualifiedType
                                }
                                    |> Just

                            _ ->
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing



--getMigrateTypes : ProjectContext a -> Dict ModuleName (Set { oldType : QualifiedType, newType : QualifiedType })
--getMigrateTypes projectContext =
--    List.foldl
--        (\( moduleName, todo ) dict ->
--            Dict.update
--                moduleName
--                (Maybe.withDefault Set.empty
--                    >> Helpers.getTypesHelper
--                        projectContext.types
--                        { oldType = todo.oldType, newType = todo.newType }
--                    >> Just
--                )
--                dict
--        )
--        Dict.empty
--        projectContext.migrateTodos


getMissingMigrations :
    ProjectContext a
    -> { oldType : QualifiedType, newType : QualifiedType }
    -> Set { oldType : QualifiedType, newType : QualifiedType }
getMissingMigrations context migrationPair =
    case
        ( QualifiedType.getTypeData context.types migrationPair.oldType
        , QualifiedType.getTypeData context.types migrationPair.newType
        )
    of
        ( Just ( _, TypeValue oldTypeData ), Just ( _, TypeValue newTypeData ) ) ->
            Set.empty

        ( Just ( _, TypeAliasValue _ oldTypeData ), Just ( _, TypeAliasValue _ newTypeData ) ) ->
            let
                oldFields : Dict String TypeAnnotation_
                oldFields =
                    Dict.fromList oldTypeData

                newFields : Dict String TypeAnnotation_
                newFields =
                    Dict.fromList newTypeData
            in
            Set.empty

        _ ->
            Set.empty



--Dict.merge
--    (\_ _ dict -> dict)
--    (\_ oldValue newValue dict ->
--        case ( oldValue, newValue ) of
--            ( Typed_ oldQualifiedType_ oldTyped, Typed_ newQualifiedType_ newTyped ) ->
--                case isMigratable oldQualifiedType_
--                getMissingMigrations
--                    context
--                    { oldType = oldQualifiedType_, newValue = newQualifiedType_ }
--    )
--    (\_ _ dict -> dict)
--    oldFields
--    newFields
--    Set.empty


migrateTypeTodoFixes :
    ProjectContext a
    -> List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
migrateTypeTodoFixes projectContext =
    []



--projectContext.migrateTodos
--    |> List.map
--        (\( currentModule, todo ) ->
--            let
--                maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
--                maybeType =
--                    QualifiedType.getTypeData projectContext.types qualifiedType
--            in
--            case ( maybeType, Dict.get currentModule projectContext.imports ) of
--                ( Just ( _, type_ ), Just { existingImports } ) ->
--                    let
--                        name =
--                            Helpers.uncapitalize (QualifiedType.name qualifiedType) ++ "Codec"
--
--                        position =
--                            { column = 0, row = 99999 + index }
--
--                        todo : MigrateTodo
--                        todo =
--                            { functionName = name
--                            , oldType = qualifiedType
--                            , newType = qualifiedType
--                            , range =
--                                { start = position
--                                , end = position
--                                }
--                            , signature =
--                                { name = Helpers.node name
--                                , typeAnnotation =
--                                    FunctionTypeAnnotation
--                                        (Typed
--                                            (Helpers.node ( [], "AutoCodec" ))
--                                            [ QualifiedType.toString currentModule existingImports qualifiedType
--                                                |> GenericType
--                                                |> Helpers.node
--                                            ]
--                                            |> Helpers.node
--                                        )
--                                        (Typed
--                                            (Helpers.node ( [], "AutoCodec" ))
--                                            [ QualifiedType.toString currentModule existingImports qualifiedType
--                                                |> GenericType
--                                                |> Helpers.node
--                                            ]
--                                            |> Helpers.node
--                                        )
--                                        |> Helpers.node
--                                }
--                            }
--
--                        ( fix, newImports ) =
--                            generateMigrationDefinition
--                                projectContext
--                                existingImports
--                                currentModule
--                                todo
--                                type_
--                    in
--                    { moduleName = currentModule
--                    , fix = Review.Fix.insertAt position fix
--                    , newImports = Set.insert (QualifiedType.qualifiedPath qualifiedType) newImports
--                    }
--                        |> Just
--
--                _ ->
--                    Nothing
--        )
--getMigrateTypes projectContext
--    |> Dict.toList
--    |> List.concatMap
--        (\( currentModule, qualifiedTypes ) ->
--            let
--                todosInModule : List MigrateTodo
--                todosInModule =
--                    List.filterMap
--                        (\( moduleName_, todo ) ->
--                            if moduleName_ == currentModule then
--                                Just todo
--
--                            else
--                                Nothing
--                        )
--                        projectContext.autoCodecTodos
--            in
--            Set.toList qualifiedTypes
--                |> List.filter (\a -> List.any (.newType >> (/=) a) todosInModule)
--                |> List.filter (\a -> List.all (.newType >> (/=) a) projectContext.migrateFunctions)
--                |> List.indexedMap
--                    (\index qualifiedType ->
--                        let
--                            maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
--                            maybeType =
--                                QualifiedType.getTypeData projectContext.types qualifiedType
--                        in
--                        case ( maybeType, Dict.get currentModule projectContext.imports ) of
--                            ( Just ( _, type_ ), Just { existingImports } ) ->
--                                let
--                                    name =
--                                        Helpers.uncapitalize (QualifiedType.name qualifiedType) ++ "Codec"
--
--                                    position =
--                                        { column = 0, row = 99999 + index }
--
--                                    todo : MigrateTodo
--                                    todo =
--                                        { functionName = name
--                                        , oldType = qualifiedType
--                                        , newType = qualifiedType
--                                        , range =
--                                            { start = position
--                                            , end = position
--                                            }
--                                        , signature =
--                                            { name = Helpers.node name
--                                            , typeAnnotation =
--                                                FunctionTypeAnnotation
--                                                    (Typed
--                                                        (Helpers.node ( [], "AutoCodec" ))
--                                                        [ QualifiedType.toString currentModule existingImports qualifiedType
--                                                            |> GenericType
--                                                            |> Helpers.node
--                                                        ]
--                                                        |> Helpers.node
--                                                    )
--                                                    (Typed
--                                                        (Helpers.node ( [], "AutoCodec" ))
--                                                        [ QualifiedType.toString currentModule existingImports qualifiedType
--                                                            |> GenericType
--                                                            |> Helpers.node
--                                                        ]
--                                                        |> Helpers.node
--                                                    )
--                                                    |> Helpers.node
--                                            }
--                                        }
--
--                                    ( fix, newImports ) =
--                                        generateMigrationDefinition
--                                            projectContext
--                                            existingImports
--                                            currentModule
--                                            todo
--                                            type_
--                                in
--                                { moduleName = currentModule
--                                , fix = Review.Fix.insertAt position fix
--                                , newImports = Set.insert (QualifiedType.qualifiedPath qualifiedType) newImports
--                                }
--                                    |> Just
--
--                            _ ->
--                                Nothing
--                    )
--                |> List.filterMap identity
--        )


todoErrors :
    ProjectContext a
    -> ModuleName
    -> List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
    -> MigrateTodo
    -> Maybe (Rule.Error scope)
todoErrors projectContext moduleName typeTodoFixes todo =
    let
        maybeOldType : Maybe ( ModuleName, TypeOrTypeAlias )
        maybeOldType =
            QualifiedType.getTypeData projectContext.types todo.oldType

        maybeNewType : Maybe ( ModuleName, TypeOrTypeAlias )
        maybeNewType =
            QualifiedType.getTypeData projectContext.types todo.newType
    in
    case ( ( maybeOldType, maybeNewType ), Dict.get moduleName projectContext.moduleKeys, Dict.get moduleName projectContext.imports ) of
        ( ( Just ( _, oldType ), Just ( _, newType ) ), Just moduleKey, Just imports ) ->
            let
                ( fix, _ ) =
                    generateMigrationDefinition
                        projectContext
                        imports.existingImports
                        moduleName
                        todo
                        oldType
                        newType
            in
            Rule.errorForModuleWithFix
                moduleKey
                { message = "Here's my attempt to complete this stub"
                , details = [ "" ]
                }
                todo.range
                (Review.Fix.replaceRangeBy todo.range fix
                    :: List.filterMap
                        (\fixes ->
                            if fixes.moduleName == moduleName then
                                Just fixes.fix

                            else
                                Nothing
                        )
                        typeTodoFixes
                )
                |> Just

        _ ->
            Nothing
