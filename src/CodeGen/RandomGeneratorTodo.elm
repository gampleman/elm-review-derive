module CodeGen.RandomGeneratorTodo exposing (ProjectContext, RandomGeneratorTodo, getTodos, randomGeneratorTypeTodoFixes, todoErrors)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import CodeGen.CodecTodo exposing (CodecTodo)
import CodeGen.Helpers as Helpers
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
        , codecs : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
        , randomGeneratorTodos : List ( ModuleName, CodecTodo )
        , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
        , moduleKeys : Dict ModuleName ModuleKey
    }


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
generateRecordRandomGenerator _ existingImports currentModule typeAliasName recordFields =
    List.foldl
        (\_ code ->
            code
                |> Helpers.pipeRight
                    (Helpers.application
                        [ Helpers.functionOrValue [ "Random" ] "andMap"

                        --, codecFromTypeAnnotation projectContext typeAnnotation
                        ]
                    )
        )
        (Helpers.application
            [ Helpers.functionOrValue [ "Random" ] "constant"
            , case typeAliasName of
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
            ]
        )
        recordFields


generateCustomTypeRandomGenerator : ProjectContext a -> Type_ -> Node Expression
generateCustomTypeRandomGenerator projectContext customType =
    case customType.constructors of
        head :: rest ->
            let
                uniformChoice : ValueConstructor_ -> Node Expression
                uniformChoice constructor =
                    case constructor.arguments of
                        [] ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "constant"
                                , Helpers.functionOrValue [] constructor.name
                                ]

                        argument :: [] ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "map"
                                , Helpers.functionOrValue [] constructor.name
                                , randomGeneratorFromTypeAnnotation projectContext argument
                                ]

                        arguments ->
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
                                                , randomGeneratorFromTypeAnnotation projectContext argument
                                                ]
                                            )
                                )
                                startCode
                                arguments

                uniformList : Node Expression
                uniformList =
                    List.map uniformChoice rest
                        |> Expression.ListExpr
                        |> Helpers.node
            in
            Helpers.application
                [ Helpers.functionOrValue [ "Random" ] "uniform", uniformChoice head |> Helpers.parenthesis, uniformList ]
                |> Helpers.pipeRight
                    (Helpers.application
                        [ Helpers.functionOrValue [ "Random" ] "andThen"
                        , Helpers.functionOrValue [] "identity"
                        ]
                    )

        [] ->
            Helpers.node Expression.UnitExpr


randomGeneratorFromTypeAnnotation : ProjectContext a -> TypeAnnotation_ -> Node Expression
randomGeneratorFromTypeAnnotation projectContext typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeVariables ->
            let
                getCodecName : String -> Node Expression
                getCodecName text =
                    case text of
                        "Int" ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "int"
                                , Expression.Integer 0 |> Helpers.node
                                , Expression.Integer 10 |> Helpers.node
                                ]

                        "Float" ->
                            Helpers.application
                                [ Helpers.functionOrValue [ "Random" ] "float"
                                , Expression.Floatable 0 |> Helpers.node
                                , Expression.Floatable 10 |> Helpers.node
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

                        _ ->
                            Helpers.functionOrValue [] ("random" ++ text)

                applied : Node Expression
                applied =
                    (case Helpers.find (.typeVar >> (==) qualifiedType) projectContext.codecs of
                        Just codec ->
                            Helpers.functionOrValue codec.moduleName codec.functionName

                        Nothing ->
                            getCodecName (QualifiedType.name qualifiedType)
                    )
                        :: List.map (randomGeneratorFromTypeAnnotation projectContext) typeVariables
                        |> Helpers.application
            in
            if List.isEmpty typeVariables then
                applied

            else
                Helpers.parenthesis applied

        Unit_ ->
            Helpers.application [ Helpers.functionOrValue [ "Random" ] "constant", Helpers.node Expression.UnitExpr ]

        Tupled_ [ first ] ->
            randomGeneratorFromTypeAnnotation projectContext first

        Tupled_ [ first, second ] ->
            Helpers.application
                [ Helpers.functionOrValue [ "Random" ] "pair"
                , randomGeneratorFromTypeAnnotation projectContext first
                , randomGeneratorFromTypeAnnotation projectContext second
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
                , randomGeneratorFromTypeAnnotation projectContext first
                , randomGeneratorFromTypeAnnotation projectContext second
                , randomGeneratorFromTypeAnnotation projectContext third
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
    -> CodecTodo
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
                        generateCustomTypeRandomGenerator projectContext typeValue

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
                    todosInModule : List CodecTodo
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
                    |> List.filter (\a -> List.all (.typeVar >> (/=) a) projectContext.codecs)
                    |> List.indexedMap
                        (\index qualifiedType ->
                            let
                                maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                maybeType =
                                    QualifiedType.getTypeData projectContext.types qualifiedType
                            in
                            case ( maybeType, Dict.get moduleName projectContext.imports ) of
                                ( Just ( _, type_ ), Just imports ) ->
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

                                _ ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
