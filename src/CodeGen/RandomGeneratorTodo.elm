module CodeGen.RandomGeneratorTodo exposing (ProjectContext, RandomGeneratorTodo, declarationVisitorGetGenerators, getTodos, randomGeneratorTypeTodoFixes, todoErrors)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import CodeGen.Helpers as Helpers
import Elm.CodeGen
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import QualifiedType exposing (ExistingImport, QualifiedType, TypeAnnotation_(..), TypeOrTypeAlias(..), Type_, ValueConstructor_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule exposing (ModuleKey)


type alias RandomGeneratorTodo =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    , parameters : List (Node Pattern)
    , signature : Signature
    }


type alias ProjectContext a =
    { a
        | types : List ( ModuleName, TypeOrTypeAlias )
        , generators : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
        , randomGeneratorTodos : List ( ModuleName, RandomGeneratorTodo )
        , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
        , moduleKeys : Dict ModuleName ModuleKey
    }


declarationVisitorGetGenerators :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Function
    -> Maybe { functionName : String, typeVar : QualifiedType }
declarationVisitorGetGenerators context function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case Helpers.typeAnnotationReturnValue signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed ((Node _ ( _, "Generator" )) as genType) [ Node _ (TypeAnnotation.Typed generatorType _) ]) ->
                    if ModuleNameLookupTable.moduleNameFor context.lookupTable genType == Just [ "Random" ] then
                        if Helpers.hasDebugTodo declaration then
                            Nothing

                        else
                            case QualifiedType.create context.lookupTable context.currentModule generatorType of
                                Just qualifiedType ->
                                    { functionName = Node.value signature.name
                                    , typeVar = qualifiedType
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


getTodos :
    { a | lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Range
    -> Function
    -> Maybe RandomGeneratorTodo
getTodos context declarationRange function =
    let
        declaration =
            Node.value function.declaration

        abc signature randomType =
            if Helpers.hasDebugTodo declaration then
                case QualifiedType.create context.lookupTable context.currentModule randomType of
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
                Node _ (TypeAnnotation.Typed (Node _ ( [], "Generator" )) [ Node _ (TypeAnnotation.Typed randomType _) ]) ->
                    abc signature randomType

                Node _ (TypeAnnotation.Typed (Node _ ( [ "Random" ], "Generator" )) [ Node _ (TypeAnnotation.Typed randomType _) ]) ->
                    abc signature randomType

                _ ->
                    Nothing

        _ ->
            Nothing


generateRecordRandomGenerator :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> Maybe QualifiedType
    -> List ( String, TypeAnnotation_ )
    -> Node Expression
generateRecordRandomGenerator projectContext existingImports currentModule typeAliasName recordFields =
    let
        numFields =
            List.length recordFields

        recordConstructor =
            case typeAliasName of
                Just typeAliasName_ ->
                    Helpers.functionOrValue
                        (QualifiedType.moduleName currentModule existingImports typeAliasName_)
                        (QualifiedType.name typeAliasName_)

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
    in
    if numFields <= 5 then
        Helpers.application
            (Helpers.functionOrValue [ "Random" ]
                (if numFields > 1 then
                    "map" ++ String.fromInt (List.length recordFields)

                 else
                    "map"
                )
                :: recordConstructor
                :: List.map (Tuple.second >> randomGeneratorFromTypeAnnotation projectContext existingImports currentModule >> Helpers.parenthesisIfNecessary) recordFields
            )

    else
        List.foldl
            (\( _, typeAnnotation ) code ->
                code
                    |> Helpers.pipeRight
                        (Helpers.application
                            [ Helpers.functionOrValue [ "Random" ] "map2"
                            , Helpers.node (Expression.PrefixOperator "|>")
                            , Helpers.parenthesisIfNecessary (randomGeneratorFromTypeAnnotation projectContext existingImports currentModule typeAnnotation)
                            ]
                        )
            )
            (Helpers.application
                [ Helpers.functionOrValue [ "Random" ] "constant"
                , recordConstructor
                ]
            )
            recordFields


generateConstructorGenerator :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> ValueConstructor_
    -> Node Expression
generateConstructorGenerator projectContext existingImports currentModule constructor =
    let
        argCount =
            List.length constructor.arguments
    in
    if argCount == 0 then
        Helpers.application
            [ Helpers.functionOrValue [ "Random" ] "constant"
            , Helpers.functionOrValue [] constructor.name
            ]

    else if argCount <= 5 then
        Helpers.application
            (Helpers.functionOrValue [ "Random" ]
                (if argCount == 1 then
                    "map"

                 else
                    "map" ++ String.fromInt argCount
                )
                :: Helpers.functionOrValue [] constructor.name
                :: List.map (randomGeneratorFromTypeAnnotation projectContext existingImports currentModule >> Helpers.parenthesisIfNecessary) constructor.arguments
            )

    else
        let
            startCode =
                Helpers.application
                    [ Helpers.functionOrValue [ "Random" ] "constant"
                    , Helpers.functionOrValue [] constructor.name
                    ]
        in
        List.foldl
            (\argument code ->
                code
                    |> Helpers.pipeRight
                        (Helpers.application
                            [ Helpers.functionOrValue [ "Random" ] "andMap"
                            , randomGeneratorFromTypeAnnotation projectContext existingImports currentModule argument
                            ]
                        )
            )
            startCode
            constructor.arguments


generateCustomTypeRandomGenerator :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> Type_
    -> Node Expression
generateCustomTypeRandomGenerator projectContext existingImports currentModule customType =
    case customType.constructors of
        -- special case for a single constructor
        [ singleCtor ] ->
            generateConstructorGenerator projectContext existingImports currentModule singleCtor

        head :: rest ->
            if List.all (.arguments >> List.isEmpty) customType.constructors then
                Helpers.application
                    [ Helpers.functionOrValue [ "Random" ] "uniform"
                    , Helpers.functionOrValue [] head.name |> Helpers.parenthesis
                    , List.map (.name >> Helpers.functionOrValue []) rest
                        |> Expression.ListExpr
                        |> Helpers.node
                    ]

            else
                let
                    uniformList : Node Expression
                    uniformList =
                        List.map (generateConstructorGenerator projectContext existingImports currentModule) rest
                            |> Expression.ListExpr
                            |> Helpers.node
                in
                Helpers.application
                    [ Helpers.functionOrValue [ "Random" ] "uniform", generateConstructorGenerator projectContext existingImports currentModule head |> Helpers.parenthesis, uniformList ]
                    |> Helpers.pipeRight
                        (Helpers.application
                            [ Helpers.functionOrValue [ "Random" ] "andThen"
                            , Helpers.functionOrValue [] "identity"
                            ]
                        )

        [] ->
            Helpers.node Expression.UnitExpr


randomGeneratorFromTypeAnnotation :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> TypeAnnotation_
    -> Node Expression
randomGeneratorFromTypeAnnotation projectContext existingImports currentModule typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeVariables ->
            let
                getGeneratorName : QualifiedType -> Node Expression
                getGeneratorName type_ =
                    case QualifiedType.name type_ of
                        "Int" ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "int"
                                , Helpers.functionOrValue [ "Random" ] "minInt"
                                , Helpers.functionOrValue [ "Random" ] "maxInt"
                                ]

                        "Float" ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "float"
                                , Expression.Floatable 0 |> Helpers.node
                                , Expression.Floatable 10 |> Helpers.node
                                ]

                        "String" ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "uniform"
                                , Expression.Literal "TODO: Define string options" |> Helpers.node
                                , Expression.ListExpr [] |> Helpers.node
                                ]

                        "List" ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "int"
                                , Expression.Integer 0 |> Helpers.node
                                , Expression.Integer 10 |> Helpers.node
                                ]
                                |> Helpers.pipeRight
                                    (Helpers.application
                                        [ Helpers.functionOrValue [ "Random" ] "andThen"
                                        , Expression.LambdaExpression
                                            { args = [ VarPattern "count" |> Helpers.node ]
                                            , expression =
                                                Helpers.application
                                                    [ Helpers.functionOrValue [ "Random" ] "list"
                                                    , Helpers.functionOrValue [] "count"
                                                    ]
                                            }
                                            |> Helpers.node
                                            |> Helpers.parenthesis
                                        ]
                                    )

                        text ->
                            if
                                Helpers.find
                                    (\( module_, types ) ->
                                        module_
                                            == currentModule
                                            && (case types of
                                                    QualifiedType.TypeValue a ->
                                                        a.qualifiedType == type_

                                                    _ ->
                                                        False
                                               )
                                    )
                                    projectContext.types
                                    == Nothing
                            then
                                Helpers.errorMessage ("Insert a `Random.Generator " ++ text ++ "` here")

                            else
                                Helpers.functionOrValue [] ("random" ++ text)
            in
            if List.isEmpty typeVariables then
                case Helpers.find (.typeVar >> (==) qualifiedType) projectContext.generators of
                    Just generator ->
                        Helpers.functionOrValue generator.moduleName generator.functionName

                    Nothing ->
                        getGeneratorName qualifiedType

            else
                (case Helpers.find (.typeVar >> (==) qualifiedType) projectContext.generators of
                    Just generator ->
                        Helpers.functionOrValue generator.moduleName generator.functionName

                    Nothing ->
                        getGeneratorName qualifiedType
                )
                    :: List.map (randomGeneratorFromTypeAnnotation projectContext existingImports currentModule) typeVariables
                    |> Helpers.application
                    |> Helpers.parenthesis

        Unit_ ->
            Helpers.application [ Helpers.functionOrValue [ "Random" ] "constant", Helpers.node Expression.UnitExpr ]

        Tupled_ [ first ] ->
            randomGeneratorFromTypeAnnotation projectContext existingImports currentModule first

        Tupled_ [ first, second ] ->
            Helpers.application
                [ Helpers.functionOrValue [ "Random" ] "pair"
                , randomGeneratorFromTypeAnnotation projectContext existingImports currentModule first
                , randomGeneratorFromTypeAnnotation projectContext existingImports currentModule second
                ]
                |> Helpers.parenthesis

        Tupled_ [ first, second, third ] ->
            Helpers.application
                [ Helpers.functionOrValue [ "Random" ] "map3"
                , Expression.LambdaExpression
                    { args = [ VarPattern "a" |> Helpers.node, VarPattern "b" |> Helpers.node, VarPattern "c" |> Helpers.node ]
                    , expression =
                        Expression.TupledExpression
                            [ Helpers.functionOrValue [] "a", Helpers.functionOrValue [] "b", Helpers.functionOrValue [] "c" ]
                            |> Helpers.node
                    }
                    |> Helpers.node
                , randomGeneratorFromTypeAnnotation projectContext existingImports currentModule first
                , randomGeneratorFromTypeAnnotation projectContext existingImports currentModule second
                , randomGeneratorFromTypeAnnotation projectContext existingImports currentModule third
                ]
                |> Helpers.parenthesis

        Tupled_ _ ->
            Helpers.application [ Helpers.functionOrValue [ "Random" ] "constant", Helpers.node Expression.UnitExpr ]

        FunctionTypeAnnotation_ _ _ ->
            Helpers.errorMessage "Functions can't be randomly generated"

        GenericType_ _ ->
            Helpers.notSupportedErrorMessage

        Record_ _ ->
            Helpers.notSupportedErrorMessage

        --generateRandomGeneratorRecord projectContext Nothing fields |> parenthesis
        GenericRecord_ _ _ ->
            Helpers.notSupportedErrorMessage


getRandomGeneratorTypes : ProjectContext a -> Dict ModuleName (Set QualifiedType)
getRandomGeneratorTypes projectContext =
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
        projectContext.randomGeneratorTodos


generateRandomGeneratorDefinition :
    ProjectContext a
    -> List ExistingImport
    -> ModuleName
    -> RandomGeneratorTodo
    -> TypeOrTypeAlias
    -> String
generateRandomGeneratorDefinition projectContext existingImports currentModule randomGeneratorTodo_ typeOrTypeAlias =
    { documentation = Nothing
    , signature = Helpers.node randomGeneratorTodo_.signature |> Just
    , declaration =
        Helpers.node
            { name = Helpers.node randomGeneratorTodo_.functionName
            , arguments = randomGeneratorTodo_.parameters
            , expression =
                case typeOrTypeAlias of
                    TypeValue typeValue ->
                        generateCustomTypeRandomGenerator projectContext existingImports currentModule typeValue

                    TypeAliasValue typeAliasName fields ->
                        generateRecordRandomGenerator projectContext existingImports currentModule (Just typeAliasName) fields
            }
    }
        |> Helpers.writeDeclaration


todoErrors projectContext moduleName typeTodoFixes todo =
    let
        maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
        maybeType =
            QualifiedType.getTypeData projectContext.types todo.typeVar
    in
    case ( maybeType, Dict.get moduleName projectContext.moduleKeys, Dict.get moduleName projectContext.imports ) of
        ( Just ( _, type_ ), Just moduleKey, Just imports ) ->
            let
                fix : String
                fix =
                    generateRandomGeneratorDefinition
                        projectContext
                        imports.existingImports
                        moduleName
                        todo
                        type_
            in
            Rule.errorForModuleWithFix
                moduleKey
                { message = "Here's my attempt to complete this stub"
                , details = [ "" ]
                }
                todo.range
                (Review.Fix.replaceRangeBy todo.range fix
                    :: List.filterMap
                        (\( moduleName_, fix_ ) ->
                            if moduleName_ == moduleName then
                                Just fix_

                            else
                                Nothing
                        )
                        typeTodoFixes
                )
                |> Just

        _ ->
            Nothing


randomGeneratorTypeTodoFixes : ProjectContext a -> List ( ModuleName, Review.Fix.Fix )
randomGeneratorTypeTodoFixes projectContext =
    getRandomGeneratorTypes projectContext
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, qualifiedTypes ) ->
                let
                    todosInModule : List RandomGeneratorTodo
                    todosInModule =
                        List.filterMap
                            (\( moduleName_, todo ) ->
                                if moduleName_ == moduleName then
                                    Just todo

                                else
                                    Nothing
                            )
                            projectContext.randomGeneratorTodos
                in
                Set.toList qualifiedTypes
                    |> List.filter (\a -> List.any (.typeVar >> (/=) a) todosInModule)
                    |> List.filter (\a -> List.all (.typeVar >> (/=) a) projectContext.generators)
                    |> List.indexedMap
                        (\index qualifiedType ->
                            let
                                maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                maybeType =
                                    QualifiedType.getTypeData projectContext.types qualifiedType
                            in
                            case ( maybeType, Dict.get moduleName projectContext.imports ) of
                                ( Just ( moduleName_, type_ ), Just imports ) ->
                                    -- We only generate a subgenerator when it's in the same module
                                    -- This is a conservative move, but since we don't have an easy way to figure out
                                    -- if the type is opaque, generating a subgenerator could easily break compilability.
                                    if moduleName == moduleName_ then
                                        let
                                            name =
                                                "random" ++ QualifiedType.name qualifiedType

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
                                                            (Helpers.node ( [ "Random" ], "Generator" ))
                                                            [ QualifiedType.name qualifiedType
                                                                |> TypeAnnotation.GenericType
                                                                |> Helpers.node
                                                            ]
                                                            |> Helpers.node
                                                    }
                                                , parameters = []
                                                }

                                            fix : String
                                            fix =
                                                generateRandomGeneratorDefinition
                                                    projectContext
                                                    imports.existingImports
                                                    moduleName
                                                    todo
                                                    type_
                                        in
                                        ( moduleName, Review.Fix.insertAt position fix )
                                            |> Just

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
