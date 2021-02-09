module TodoItForMe exposing (rule)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Writer
import QualifiedType exposing (QualifiedType, TypeAnnotation_(..), TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, ModuleKey, Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "TodoItForMe" initialProjectContext
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule |> Rule.withModuleNameLookupTable |> Rule.withMetadata
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema



-- MODULE VISITOR


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationVisitor



-- CONTEXT


type alias ProjectContext =
    { types : List ( ModuleName, TypeOrTypeAlias )
    , codecs : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
    , todos : List ( ModuleName, Todo )
    , moduleKeys : Dict ModuleName ModuleKey
    }


type alias CodecTodoData =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    , parameters : List (Node Pattern)
    , signature : Signature
    }


type alias ToStringTodoData =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    , parameterName : String
    }


type alias ListAllTodoData =
    { functionName : String
    , typeVar : QualifiedType
    , range : Range
    }


type Todo
    = CodecTodo CodecTodoData
    | ToStringTodo ToStringTodoData
    | FromStringTodo ToStringTodoData
    | ListAllTodo ListAllTodoData
    | RandomGeneratorTodo  CodecTodoData

type alias IntermediateTodo =
    { functionName : Node String, typeVar : QualifiedType }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable.ModuleNameLookupTable
    , types : List TypeOrTypeAlias
    , codecs : List { functionName : String, typeVar : QualifiedType }
    , todos : List Todo
    , currentModule : ModuleName
    , moduleLookupTable : ModuleNameLookupTable
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { types = []
    , codecs = []
    , todos = []
    , moduleKeys = Dict.empty
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    let
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { lookupTable = lookupTable
    , types = projectContext.types |> List.filter (Tuple.first >> (==) moduleName) |> List.map Tuple.second
    , codecs =
        List.map
            (\a -> { functionName = a.functionName, typeVar = a.typeVar })
            projectContext.codecs
    , todos =
        List.filterMap
            (\( moduleName_, todo ) ->
                if moduleName_ == moduleName then
                    Just todo

                else
                    Nothing
            )
            projectContext.todos
    , currentModule = moduleName
    , moduleLookupTable = lookupTable
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    let
        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { types = List.map (Tuple.pair moduleName) moduleContext.types
    , codecs =
        List.map
            (\a -> { moduleName = moduleName, functionName = a.functionName, typeVar = a.typeVar })
            moduleContext.codecs
    , todos = List.map (\todo -> ( moduleName, todo )) moduleContext.todos
    , moduleKeys = Dict.singleton moduleName moduleKey
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { types = newContext.types ++ previousContext.types
    , codecs = newContext.codecs ++ previousContext.codecs
    , todos = newContext.todos ++ previousContext.todos
    , moduleKeys = Dict.union newContext.moduleKeys previousContext.moduleKeys
    }



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case maybeElmJson |> Maybe.map .project of
        Just (Elm.Project.Package package) ->
            ( [], projectContext )

        Just (Elm.Project.Application _) ->
            ( [], projectContext )

        Nothing ->
            ( [], projectContext )



-- DECLARATION VISITOR


convertTypeAnnotation : ModuleContext -> TypeAnnotation -> TypeAnnotation_
convertTypeAnnotation moduleContext typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.GenericType string ->
            QualifiedType.GenericType_ string

        TypeAnnotation.Typed a nodes ->
            case QualifiedType.create moduleContext.moduleLookupTable moduleContext.currentModule a of
                Just qualified ->
                    QualifiedType.Typed_
                        qualified
                        (List.map (Node.value >> convertTypeAnnotation moduleContext) nodes)

                Nothing ->
                    QualifiedType.Unit_

        TypeAnnotation.Unit ->
            QualifiedType.Unit_

        TypeAnnotation.Tupled nodes ->
            QualifiedType.Tupled_ (List.map (Node.value >> convertTypeAnnotation moduleContext) nodes)

        TypeAnnotation.Record recordDefinition ->
            QualifiedType.Record_ (convertRecordDefinition moduleContext recordDefinition)

        TypeAnnotation.GenericRecord a (Node _ recordDefinition) ->
            QualifiedType.GenericRecord_
                (Node.value a)
                (convertRecordDefinition moduleContext recordDefinition)

        TypeAnnotation.FunctionTypeAnnotation (Node _ a) (Node _ b) ->
            QualifiedType.FunctionTypeAnnotation_
                (convertTypeAnnotation moduleContext a)
                (convertTypeAnnotation moduleContext b)


convertRecordDefinition : ModuleContext -> List (Node ( Node a, Node TypeAnnotation )) -> List ( a, TypeAnnotation_ )
convertRecordDefinition moduleContext recordDefinition =
    List.map
        (\(Node _ ( Node _ fieldName, Node _ fieldValue )) ->
            ( fieldName, convertTypeAnnotation moduleContext fieldValue )
        )
        recordDefinition


convertType : ModuleContext -> Elm.Syntax.Type.Type -> Type_
convertType moduleContext type_ =
    { name = Node.value type_.name
    , generics = List.map Node.value type_.generics
    , constructors =
        List.map
            (\(Node _ constructor) ->
                { name = Node.value constructor.name
                , arguments =
                    List.map
                        (Node.value >> convertTypeAnnotation moduleContext)
                        constructor.arguments
                }
            )
            type_.constructors
    }


declarationVisitor : List (Node Declaration) -> ModuleContext -> ( List a, ModuleContext )
declarationVisitor declarations context =
    let
        context2 =
            { context
                | types =
                    List.filterMap
                        (declarationVisitorGetTypes context)
                        declarations
                        ++ context.types
                , codecs =
                    List.filterMap
                        (\declaration ->
                            case declaration of
                                Node _ (Declaration.FunctionDeclaration function) ->
                                    declarationVisitorGetCodecs context function

                                _ ->
                                    Nothing
                        )
                        declarations
                        ++ context.codecs
            }
    in
    ( []
    , { context2
        | todos =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node range (Declaration.FunctionDeclaration function) ->
                            getCodecTodo context range function
                                |> orMaybe (getToStringTodo context range function)
                                |> orMaybe (getFromStringTodo context range function)
                                |> orMaybe (getListAllTodo context range function)
                                |> orMaybe (randomGeneratorTodo context range function)

                        _ ->
                            Nothing
                )
                declarations
                ++ context.todos
      }
    )


orMaybe : Maybe a -> Maybe a -> Maybe a
orMaybe maybeA maybeB =
    case maybeB of
        Just b ->
            Just b

        Nothing ->
            maybeA


declarationVisitorGetCodecs : ModuleContext -> Function -> Maybe { functionName : String, typeVar : QualifiedType }
declarationVisitorGetCodecs context function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case typeAnnotationReturnValue signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ _, Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    if hasDebugTodo declaration then
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


declarationVisitorGetTypes : ModuleContext -> Node Declaration -> Maybe TypeOrTypeAlias
declarationVisitorGetTypes context node_ =
    case Node.value node_ of
        Declaration.CustomTypeDeclaration customType ->
            TypeValue (convertType context customType) |> Just

        Declaration.AliasDeclaration typeAlias ->
            case Node.value typeAlias.typeAnnotation of
                TypeAnnotation.Record record ->
                    TypeAliasValue
                        (Node.value typeAlias.name)
                        (convertRecordDefinition context record)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


typeAnnotationReturnValue : Node TypeAnnotation -> Node TypeAnnotation
typeAnnotationReturnValue typeAnnotation =
    case typeAnnotation of
        Node _ (TypeAnnotation.FunctionTypeAnnotation _ node_) ->
            typeAnnotationReturnValue node_

        _ ->
            typeAnnotation


getCodecTodo : ModuleContext -> Range -> Function -> Maybe Todo
getCodecTodo context declarationRange function =
    let
        declaration =
            Node.value function.declaration

        abc signature codecType =
            if hasDebugTodo declaration then
                case QualifiedType.create context.lookupTable context.currentModule codecType of
                    Just qualifiedType ->
                        CodecTodo
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
            case typeAnnotationReturnValue signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ _, Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    abc signature codecType

                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    abc signature codecType

                Node _ (TypeAnnotation.Typed (Node _ ( [ "Serialize" ], "Codec" )) [ Node _ _, Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    abc signature codecType

                Node _ (TypeAnnotation.Typed (Node _ ( [ "Serialize" ], "Codec" )) [ Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    abc signature codecType

                _ ->
                    Nothing

        _ ->
            Nothing


getToStringTodo : ModuleContext -> Range -> Function -> Maybe Todo
getToStringTodo context declarationRange function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case signature.typeAnnotation of
                Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed customType _)) (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) []))) ->
                    if hasDebugTodo declaration then
                        case QualifiedType.create context.lookupTable context.currentModule customType of
                            Just qualifiedType ->
                                case declaration.arguments of
                                    (Node _ (VarPattern parameter)) :: [] ->
                                        { functionName = Node.value signature.name
                                        , typeVar = qualifiedType
                                        , range = declarationRange
                                        , parameterName = parameter
                                        }
                                            |> ToStringTodo
                                            |> Just

                                    [] ->
                                        { functionName = Node.value signature.name
                                        , typeVar = qualifiedType
                                        , range = declarationRange
                                        , parameterName = "value"
                                        }
                                            |> ToStringTodo
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


getFromStringTodo : ModuleContext -> Range -> Function -> Maybe Todo
getFromStringTodo context declarationRange function =
    let
        declaration : Expression.FunctionImplementation
        declaration =
            Node.value function.declaration

        createTodo : Signature -> Node ( ModuleName, String ) -> Maybe Todo
        createTodo signature customType =
            if hasDebugTodo declaration then
                case QualifiedType.create context.lookupTable context.currentModule customType of
                    Just qualifiedType ->
                        case declaration.arguments of
                            (Node _ (VarPattern parameter)) :: [] ->
                                { functionName = Node.value signature.name
                                , typeVar = qualifiedType
                                , range = declarationRange
                                , parameterName = parameter
                                }
                                    |> FromStringTodo
                                    |> Just

                            [] ->
                                { functionName = Node.value signature.name
                                , typeVar = qualifiedType
                                , range = declarationRange
                                , parameterName = "text"
                                }
                                    |> FromStringTodo
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
                (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( [], "Maybe" )) [ Node _ (TypeAnnotation.Typed customType _) ]))) ->
                    createTodo signature customType

                (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) [])) (Node _ (TypeAnnotation.Typed customType _))) ->
                    createTodo signature customType

                _ ->
                    Nothing

        Nothing ->
            Nothing


getListAllTodo : ModuleContext -> Range -> Function -> Maybe Todo
getListAllTodo context declarationRange function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            case signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "List" )) [ Node _ (TypeAnnotation.Typed codecType _) ]) ->
                    if hasDebugTodo declaration then
                        case QualifiedType.create context.lookupTable context.currentModule codecType of
                            Just qualifiedType ->
                                ListAllTodo
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



randomGeneratorTodo : ModuleContext -> Range -> Function -> Maybe Todo
randomGeneratorTodo context declarationRange function =
    let
        declaration =
            Node.value function.declaration

        abc signature randomType =
            if hasDebugTodo declaration then
                case QualifiedType.create context.lookupTable context.currentModule randomType of
                    Just qualifiedType ->
                        RandomGeneratorTodo
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
            case typeAnnotationReturnValue signature.typeAnnotation of
                Node _ (TypeAnnotation.Typed (Node _ ( [], "Generator" )) [ Node _ (TypeAnnotation.Typed randomType _) ]) ->
                    abc signature randomType

                Node _ (TypeAnnotation.Typed (Node _ ( [ "Random" ], "Generator" )) [ Node _ (TypeAnnotation.Typed randomType _) ]) ->
                    abc signature randomType

                _ ->
                    Nothing

        _ ->
            Nothing

hasDebugTodo : { a | expression : Node Expression } -> Bool
hasDebugTodo declaration =
    case declaration.expression of
        Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
            True

        _ ->
            False


generateRecordCodec : ProjectContext -> Maybe String -> List ( String, TypeAnnotation_ ) -> Node Expression
generateRecordCodec projectContext typeAliasName recordFields =
    List.foldl
        (\( fieldName, typeAnnotation ) code ->
            code
                |> pipeLeft
                    (application
                        [ functionOrValue [ "Serialize" ] "field"
                        , Expression.RecordAccessFunction fieldName |> node
                        , codecFromTypeAnnotation projectContext typeAnnotation
                        ]
                    )
        )
        (application
            [ functionOrValue [ "Serialize" ] "record"
            , case typeAliasName of
                Just typeAliasName_ ->
                    functionOrValue [] typeAliasName_

                Nothing ->
                    Expression.LambdaExpression
                        { args =
                            List.range 0 (List.length recordFields - 1)
                                |> List.map (varFromInt >> VarPattern >> node)
                        , expression =
                            List.indexedMap
                                (\index ( fieldName, _ ) ->
                                    ( node fieldName, functionOrValue [] (varFromInt index) ) |> node
                                )
                                recordFields
                                |> Expression.RecordExpr
                                |> node
                        }
                        |> node
                        |> parenthesis
            ]
        )
        recordFields
        |> pipeLeft (application [ functionOrValue [ "Serialize" ] "finishRecord" ])


varFromInt =
    (+) (Char.toCode 'a')
        >> Char.fromCode
        >> String.fromChar


generateCustomTypeCodec : ProjectContext -> Type_ -> Node Expression
generateCustomTypeCodec projectContext customType =
    let
        args : List ( Node Pattern, ( Node Pattern, Node Expression ) )
        args =
            customType.constructors
                |> List.indexedMap
                    (\index constructor ->
                        let
                            var =
                                varFromInt index

                            arguments =
                                List.range 0 (List.length constructor.arguments - 1)
                                    |> List.map (String.fromInt >> (++) "data")
                        in
                        ( VarPattern var |> node
                        , ( NamedPattern
                                { moduleName = [], name = constructor.name }
                                (List.map (VarPattern >> node) arguments)
                                |> node
                          , application (functionOrValue [] var :: List.map (functionOrValue []) arguments)
                          )
                        )
                    )

        start =
            application
                [ functionOrValue [ "Serialize" ] "customType"
                , Expression.LambdaExpression
                    { args = List.map Tuple.first args ++ [ VarPattern "value" |> node ]
                    , expression =
                        Expression.CaseExpression
                            { expression = functionOrValue [] "value", cases = List.map Tuple.second args }
                            |> node
                    }
                    |> node
                    |> parenthesis
                ]
    in
    List.foldl
        (\constructor code ->
            code
                |> pipeLeft
                    (application
                        (functionOrValue [ "Serialize" ] ("variant" ++ String.fromInt (List.length constructor.arguments))
                            :: functionOrValue [] constructor.name
                            :: List.map
                                (codecFromTypeAnnotation projectContext)
                                constructor.arguments
                        )
                    )
        )
        start
        customType.constructors
        |> pipeLeft (application [ functionOrValue [ "Serialize" ] "finishCustomType" ])


generateCodecTodoDefinition : ProjectContext -> CodecTodoData -> TypeOrTypeAlias -> String
generateCodecTodoDefinition projectContext codecTodo typeOrTypeAlias =
    { documentation = Nothing
    , signature = node codecTodo.signature |> Just
    , declaration =
        node
            { name = node codecTodo.functionName
            , arguments = codecTodo.parameters
            , expression =
                case typeOrTypeAlias of
                    TypeValue typeValue ->
                        generateCustomTypeCodec projectContext typeValue

                    TypeAliasValue typeAliasName fields ->
                        generateRecordCodec projectContext (Just typeAliasName) fields
            }
    }
        |> writeDeclaration


generateToStringDefinition : ToStringTodoData -> Type_ -> String
generateToStringDefinition toStringTodo type_ =
    { documentation = Nothing
    , signature =
        { name = node toStringTodo.functionName
        , typeAnnotation =
            TypeAnnotation.FunctionTypeAnnotation
                (TypeAnnotation.Typed (node ( [], QualifiedType.name toStringTodo.typeVar )) []
                    |> node
                )
                (TypeAnnotation.Typed (node ( [], "String" )) [] |> node)
                |> node
        }
            |> node
            |> Just
    , declaration =
        node
            { name = node toStringTodo.functionName
            , arguments = [ VarPattern toStringTodo.parameterName |> node ]
            , expression =
                Expression.CaseExpression
                    { expression = functionOrValue [] toStringTodo.parameterName
                    , cases =
                        List.map
                            (\constructor ->
                                ( NamedPattern
                                    { moduleName = []
                                    , name = constructor.name
                                    }
                                    (List.repeat (List.length constructor.arguments) (node AllPattern))
                                    |> node
                                , Expression.Literal constructor.name |> node
                                )
                            )
                            type_.constructors
                    }
                    |> node
            }
    }
        |> writeDeclaration


generateFromStringDefinition : ToStringTodoData -> Type_ -> String
generateFromStringDefinition toStringTodo type_ =
    { documentation = Nothing
    , signature =
        { name = node toStringTodo.functionName
        , typeAnnotation =
            TypeAnnotation.FunctionTypeAnnotation
                (TypeAnnotation.Typed (node ( [], "String" )) [] |> node)
                (TypeAnnotation.Typed
                    (node ( [], "Maybe" ))
                    [ TypeAnnotation.Typed (node ( [], QualifiedType.name toStringTodo.typeVar )) [] |> node ]
                    |> node
                )
                |> node
        }
            |> node
            |> Just
    , declaration =
        node
            { name = node toStringTodo.functionName
            , arguments = [ VarPattern toStringTodo.parameterName |> node ]
            , expression =
                Expression.CaseExpression
                    { expression = functionOrValue [] toStringTodo.parameterName
                    , cases =
                        List.map
                            (\constructor ->
                                ( StringPattern constructor.name |> node
                                , if List.isEmpty constructor.arguments then
                                    application
                                        [ functionOrValue [] "Just"
                                        , functionOrValue [] constructor.name
                                        ]

                                  else
                                    Expression.OperatorApplication "|>"
                                        Right
                                        (functionOrValue [] constructor.name
                                            :: List.repeat (List.length constructor.arguments) notSupportedErrorMessage
                                            |> application
                                        )
                                        (functionOrValue [] "Just")
                                        |> node
                                )
                            )
                            type_.constructors
                            ++ [ ( node AllPattern, functionOrValue [] "Nothing" ) ]
                    }
                    |> node
            }
    }
        |> writeDeclaration


generateListAllDefinition : ListAllTodoData -> Type_ -> String
generateListAllDefinition toStringTodo type_ =
    { documentation = Nothing
    , signature =
        { name = node toStringTodo.functionName
        , typeAnnotation =
            TypeAnnotation.Typed
                (node ( [], "List" ))
                [ TypeAnnotation.Typed
                    (node ( [], QualifiedType.name toStringTodo.typeVar ))
                    []
                    |> node
                ]
                |> node
        }
            |> node
            |> Just
    , declaration =
        node
            { name = node toStringTodo.functionName
            , arguments = []
            , expression =
                List.map
                    (\constructor ->
                        functionOrValue [] constructor.name
                            :: List.repeat
                                (List.length constructor.arguments)
                                notSupportedErrorMessage
                            |> application
                    )
                    type_.constructors
                    |> Expression.ListExpr
                    |> node
            }
    }
        |> writeDeclaration
        |> String.replace "," "\n    ,"

generateRandomGeneratorDefinition : ProjectContext -> CodecTodoData -> TypeOrTypeAlias -> String
generateRandomGeneratorDefinition projectContext randomGeneratorTodo_ typeOrTypeAlias =
    { documentation = Nothing
    , signature = node randomGeneratorTodo_.signature |> Just
    , declaration =
        node
            { name = node randomGeneratorTodo_.functionName
            , arguments = randomGeneratorTodo_.parameters
            , expression =
                case typeOrTypeAlias of
                    TypeValue typeValue ->
                        generateCustomTypeRandomGenerator projectContext typeValue

                    TypeAliasValue typeAliasName fields ->
                        generateRecordCodec projectContext (Just typeAliasName) fields
            }
    }
        |> writeDeclaration


generateCustomTypeRandomGenerator : ProjectContext -> Type_ -> Node Expression
generateCustomTypeRandomGenerator projectContext customType =
    let
        args : List ( Node Pattern, ( Node Pattern, Node Expression ) )
        args =
            customType.constructors
                |> List.indexedMap
                    (\index constructor ->
                        let
                            var =
                                varFromInt index

                            arguments =
                                List.range 0 (List.length constructor.arguments - 1)
                                    |> List.map (String.fromInt >> (++) "data")
                        in
                        ( VarPattern var |> node
                        , ( NamedPattern
                                { moduleName = [], name = constructor.name }
                                (List.map (VarPattern >> node) arguments)
                                |> node
                          , application (functionOrValue [] var :: List.map (functionOrValue []) arguments)
                          )
                        )
                    )

        start =
            application
                [ functionOrValue [ "Serialize" ] "customType"
                , Expression.LambdaExpression
                    { args = List.map Tuple.first args ++ [ VarPattern "value" |> node ]
                    , expression =
                        Expression.CaseExpression
                            { expression = functionOrValue [] "value", cases = List.map Tuple.second args }
                            |> node
                    }
                    |> node
                    |> parenthesis
                ]
    in
    case customType.constructors of
        head :: rest ->
            let
                uniformChoice constructor =
                    case constructor.arguments of
                        [] ->
                            application
                                    [ functionOrValue [ "Random"] "constant"
                                    , functionOrValue [] constructor.name
                                    ]

                        arguments ->
                            let
                                startCode =
                                    application
                                        [ functionOrValue [ "Random"] "constant"
                                        , functionOrValue [] constructor.name]
                            in
                            List.foldl
                                    (\argument code ->
                                        code
                                            |> pipeLeft
                                                (application
                                                    (functionOrValue [  ] ("andRandomMap")
                                                        :: functionOrValue [] argument
                                                        :: List.map
                                                            (randomGeneratorFromTypeAnnotation projectContext)
                                                            constructor.arguments
                                                    )
                                                )
                                    )
                                    startCode
                                    arguments




                uniformList = List.map uniformChoice rest
                                    |> Expression.ListExpr
                                    |> node
            in
            application ((functionOrValue [ "Random"] "uniform")
                :: (uniformChoice head |> parenthesis)
                 :: uniformList
                 :: [])
                 |> pipeLeft (application [
                    functionOrValue [ "Random"] "andThen",
                    functionOrValue [] "identity" ] )

        [] -> node Expression.UnitExpr

randomGeneratorFromTypeAnnotation : ProjectContext -> TypeAnnotation_ -> Node Expression
randomGeneratorFromTypeAnnotation projectContext typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeVariables ->
            let
                getCodecName : String -> Node Expression
                getCodecName text =
                    case text of
                        "Int" ->
                            application [ functionOrValue [ "Random" ] "int"
                                        , Expression.Integer 0 |> node
                                        , Expression.Integer 10 |> node    ]

                        "Float" ->
                            application [ functionOrValue [ "Random" ] "float"
                                        , Expression.Integer 0 |> node
                                        , Expression.Integer 10 |> node
                                        ]

                        "List" ->
                            functionOrValue [ "Serialize" ] "list"

                        _ ->
                            functionOrValue [] ("random" ++ text)

                applied : Node Expression
                applied =
                    (case find (.typeVar >> (==) qualifiedType) projectContext.codecs of
                        Just codec ->
                            functionOrValue codec.moduleName codec.functionName

                        Nothing ->
                            getCodecName (QualifiedType.name qualifiedType)
                    )
                        :: List.map (codecFromTypeAnnotation projectContext) typeVariables
                        |> application
            in
            if List.isEmpty typeVariables then
                applied

            else
                parenthesis applied

        Unit_ ->
            functionOrValue [ "Serialize" ] "unit"

        Tupled_ [ first ] ->
            codecFromTypeAnnotation projectContext first

        Tupled_ [ first, second ] ->
            application
                [ functionOrValue [ "Serialize" ] "tuple"
                , codecFromTypeAnnotation projectContext first
                , codecFromTypeAnnotation projectContext second
                ]
                |> parenthesis

        Tupled_ [ first, second, third ] ->
            application
                [ functionOrValue [ "Serialize" ] "triple"
                , codecFromTypeAnnotation projectContext first
                , codecFromTypeAnnotation projectContext second
                , codecFromTypeAnnotation projectContext third
                ]
                |> parenthesis

        Tupled_ _ ->
            functionOrValue [ "Serialize" ] "unit"

        FunctionTypeAnnotation_ _ _ ->
            errorMessage "Functions can't be serialized"

        GenericType_ _ ->
            notSupportedErrorMessage

        Record_ fields ->
            generateRecordCodec projectContext Nothing fields |> parenthesis

        GenericRecord_ _ _ ->
            notSupportedErrorMessage



writeDeclaration =
    Declaration.FunctionDeclaration
        >> node
        >> Elm.Writer.writeDeclaration
        >> Elm.Writer.write
        >> String.replace "|>" "\n        |>"


codecFromTypeAnnotation : ProjectContext -> TypeAnnotation_ -> Node Expression
codecFromTypeAnnotation projectContext typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeVariables ->
            let
                getCodecName : String -> Node Expression
                getCodecName text =
                    case text of
                        "Int" ->
                            functionOrValue [ "Serialize" ] "int"

                        "Float" ->
                            functionOrValue [ "Serialize" ] "float"

                        "String" ->
                            functionOrValue [ "Serialize" ] "string"

                        "Bool" ->
                            functionOrValue [ "Serialize" ] "bool"

                        "Maybe" ->
                            functionOrValue [ "Serialize" ] "maybe"

                        "Dict" ->
                            functionOrValue [ "Serialize" ] "dict"

                        "Set" ->
                            functionOrValue [ "Serialize" ] "set"

                        "Result" ->
                            functionOrValue [ "Serialize" ] "result"

                        "List" ->
                            functionOrValue [ "Serialize" ] "list"

                        "Array" ->
                            functionOrValue [ "Serialize" ] "array"

                        _ ->
                            functionOrValue [] (uncapitalize text ++ "Codec")

                applied : Node Expression
                applied =
                    (case find (.typeVar >> (==) qualifiedType) projectContext.codecs of
                        Just codec ->
                            functionOrValue codec.moduleName codec.functionName

                        Nothing ->
                            getCodecName (QualifiedType.name qualifiedType)
                    )
                        :: List.map (codecFromTypeAnnotation projectContext) typeVariables
                        |> application
            in
            if List.isEmpty typeVariables then
                applied

            else
                parenthesis applied

        Unit_ ->
            functionOrValue [ "Serialize" ] "unit"

        Tupled_ [ first ] ->
            codecFromTypeAnnotation projectContext first

        Tupled_ [ first, second ] ->
            application
                [ functionOrValue [ "Serialize" ] "tuple"
                , codecFromTypeAnnotation projectContext first
                , codecFromTypeAnnotation projectContext second
                ]
                |> parenthesis

        Tupled_ [ first, second, third ] ->
            application
                [ functionOrValue [ "Serialize" ] "triple"
                , codecFromTypeAnnotation projectContext first
                , codecFromTypeAnnotation projectContext second
                , codecFromTypeAnnotation projectContext third
                ]
                |> parenthesis

        Tupled_ _ ->
            functionOrValue [ "Serialize" ] "unit"

        FunctionTypeAnnotation_ _ _ ->
            errorMessage "Functions can't be serialized"

        GenericType_ _ ->
            notSupportedErrorMessage

        Record_ fields ->
            generateRecordCodec projectContext Nothing fields |> parenthesis

        GenericRecord_ _ _ ->
            notSupportedErrorMessage


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


getCodecTypes : ProjectContext -> Dict ModuleName (Set QualifiedType)
getCodecTypes projectContext =
    List.foldl
        (\( moduleName, todo ) dict ->
            case todo of
                CodecTodo codecTodo ->
                    Dict.update
                        moduleName
                        (Maybe.withDefault Set.empty
                            >> getTypesHelper
                                projectContext
                                codecTodo.typeVar
                            >> Just
                        )
                        dict

                ToStringTodo _ ->
                    dict

                FromStringTodo _ ->
                    dict

                ListAllTodo _ ->
                    dict

                RandomGeneratorTodo _ ->
                    dict
        )
        Dict.empty
        projectContext.todos


getTypesFromTypeAnnotation : ProjectContext -> ModuleName -> Set QualifiedType -> TypeAnnotation_ -> Set QualifiedType
getTypesFromTypeAnnotation projectContext typeModuleName collectedTypes typeAnnotation =
    case typeAnnotation of
        Typed_ qualifiedType typeParameters ->
            let
                collectedTypes_ =
                    if QualifiedType.isPrimitiveType qualifiedType then
                        collectedTypes

                    else if Set.member qualifiedType collectedTypes then
                        collectedTypes

                    else
                        getTypesHelper
                            projectContext
                            qualifiedType
                            (Set.insert qualifiedType collectedTypes)
                            |> Set.union collectedTypes
            in
            List.foldl
                (\typeParameter collectedTypes__ ->
                    getTypesFromTypeAnnotation projectContext typeModuleName collectedTypes__ typeParameter
                        |> Set.union collectedTypes__
                )
                collectedTypes_
                typeParameters

        _ ->
            collectedTypes


getTypesHelper :
    ProjectContext
    -> QualifiedType
    -> Set QualifiedType
    -> Set QualifiedType
getTypesHelper projectContext typeDeclaration collectedTypes =
    case QualifiedType.getTypeData projectContext.types typeDeclaration of
        Just ( typeModuleName, TypeAliasValue _ fields ) ->
            List.foldl
                (\( _, typeAnnotation ) collectedTypes_ ->
                    getTypesFromTypeAnnotation projectContext typeModuleName collectedTypes_ typeAnnotation
                )
                collectedTypes
                fields

        Just ( typeModuleName, TypeValue customType ) ->
            List.foldl
                (\constructor collectedTypes_ ->
                    List.foldl
                        (\typeAnnotation collectedTypes__ ->
                            getTypesFromTypeAnnotation projectContext typeModuleName collectedTypes__ typeAnnotation
                        )
                        collectedTypes_
                        constructor.arguments
                )
                collectedTypes
                customType.constructors

        Nothing ->
            collectedTypes


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    let
        typeTodos : Dict ModuleName (Set QualifiedType)
        typeTodos =
            getCodecTypes projectContext

        todoFixes : List (Error { useErrorForModule : () })
        todoFixes =
            projectContext.todos
                |> List.filterMap
                    (\( moduleName, todo ) ->
                        case todo of
                            CodecTodo codecTodo ->
                                let
                                    maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                    maybeType =
                                        QualifiedType.getTypeData projectContext.types codecTodo.typeVar
                                in
                                case ( maybeType, Dict.get moduleName projectContext.moduleKeys ) of
                                    ( Just ( _, type_ ), Just moduleKey ) ->
                                        let
                                            fix : String
                                            fix =
                                                generateCodecTodoDefinition projectContext codecTodo type_
                                        in
                                        Rule.errorForModuleWithFix
                                            moduleKey
                                            { message = "Here's my attempt to complete this stub"
                                            , details = [ "" ]
                                            }
                                            codecTodo.range
                                            (Review.Fix.replaceRangeBy codecTodo.range fix
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

                            ToStringTodo toStringTodo ->
                                let
                                    maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                    maybeType =
                                        QualifiedType.getTypeData projectContext.types toStringTodo.typeVar
                                in
                                case ( maybeType, Dict.get moduleName projectContext.moduleKeys ) of
                                    ( Just ( _, TypeValue type_ ), Just moduleKey ) ->
                                        let
                                            fix =
                                                generateToStringDefinition toStringTodo type_
                                        in
                                        Rule.errorForModuleWithFix
                                            moduleKey
                                            { message = "Here's my attempt to complete this stub"
                                            , details = [ "" ]
                                            }
                                            toStringTodo.range
                                            [ Review.Fix.replaceRangeBy toStringTodo.range fix ]
                                            |> Just

                                    _ ->
                                        Nothing

                            FromStringTodo toStringTodo ->
                                let
                                    maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                    maybeType =
                                        QualifiedType.getTypeData projectContext.types toStringTodo.typeVar
                                in
                                case ( maybeType, Dict.get moduleName projectContext.moduleKeys ) of
                                    ( Just ( _, TypeValue type_ ), Just moduleKey ) ->
                                        let
                                            fix =
                                                generateFromStringDefinition toStringTodo type_
                                        in
                                        Rule.errorForModuleWithFix
                                            moduleKey
                                            { message = "Here's my attempt to complete this stub"
                                            , details = [ "" ]
                                            }
                                            toStringTodo.range
                                            [ Review.Fix.replaceRangeBy toStringTodo.range fix ]
                                            |> Just

                                    _ ->
                                        Nothing

                            ListAllTodo listAllTodo ->
                                let
                                    maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                    maybeType =
                                        QualifiedType.getTypeData projectContext.types listAllTodo.typeVar
                                in
                                case ( maybeType, Dict.get moduleName projectContext.moduleKeys ) of
                                    ( Just ( _, TypeValue type_ ), Just moduleKey ) ->
                                        let
                                            fix =
                                                generateListAllDefinition listAllTodo type_
                                        in
                                        Rule.errorForModuleWithFix
                                            moduleKey
                                            { message = "Here's my attempt to complete this stub"
                                            , details = [ "" ]
                                            }
                                            listAllTodo.range
                                            [ Review.Fix.replaceRangeBy listAllTodo.range fix ]
                                            |> Just

                                    _ ->
                                        Nothing


                            RandomGeneratorTodo randomTodoData ->
                                let
                                    maybeType : Maybe ( ModuleName, TypeOrTypeAlias )
                                    maybeType =
                                        QualifiedType.getTypeData projectContext.types randomTodoData.typeVar
                                in
                                case ( maybeType, Dict.get moduleName projectContext.moduleKeys ) of
                                    ( Just ( _, type_ ), Just moduleKey ) ->
                                        let
                                            fix : String
                                            fix =
                                                generateRandomGeneratorDefinition projectContext randomTodoData type_
                                        in
                                        Rule.errorForModuleWithFix
                                            moduleKey
                                            { message = "Here's my attempt to complete this stub"
                                            , details = [ "" ]
                                            }
                                            randomTodoData.range
                                            (Review.Fix.replaceRangeBy randomTodoData.range fix
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
                )

        typeTodoFixes : List ( ModuleName, Review.Fix.Fix )
        typeTodoFixes =
            typeTodos
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, qualifiedTypes ) ->
                        let
                            todosInModule : List CodecTodoData
                            todosInModule =
                                List.filterMap
                                    (\( moduleName_, todo ) ->
                                        case todo of
                                            CodecTodo codecTodo ->
                                                if moduleName_ == moduleName then
                                                    Just codecTodo

                                                else
                                                    Nothing

                                            ToStringTodo _ ->
                                                Nothing

                                            FromStringTodo _ ->
                                                Nothing

                                            ListAllTodo _ ->
                                                Nothing

                                            RandomGeneratorTodo _ ->
                                                Nothing
                                    )
                                    projectContext.todos
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
                                    case maybeType of
                                        Just ( _, type_ ) ->
                                            let
                                                name =
                                                    uncapitalize (QualifiedType.name qualifiedType) ++ "Codec"

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
                                                        { name = node name
                                                        , typeAnnotation =
                                                            TypeAnnotation.Typed
                                                                (node ( [], "Codec" ))
                                                                [ TypeAnnotation.GenericType "e" |> node
                                                                , QualifiedType.name qualifiedType
                                                                    |> TypeAnnotation.GenericType
                                                                    |> node
                                                                ]
                                                                |> node
                                                        }
                                                    , parameters = []
                                                    }

                                                fix : String
                                                fix =
                                                    generateCodecTodoDefinition projectContext todo type_
                                            in
                                            ( moduleName, Review.Fix.insertAt position fix )
                                                |> Just

                                        Nothing ->
                                            Nothing
                                )
                            |> List.filterMap identity
                    )
    in
    todoFixes


node =
    Node Elm.Syntax.Range.emptyRange


application =
    Expression.Application >> node


functionOrValue : ModuleName -> String -> Node Expression
functionOrValue moduleName functionOrValueName =
    Expression.FunctionOrValue moduleName functionOrValueName |> node


pipeLeft : Node Expression -> Node Expression -> Node Expression
pipeLeft eRight eLeft =
    Expression.OperatorApplication "|>" Infix.Left eLeft eRight |> node


parenthesis : Node Expression -> Node Expression
parenthesis =
    Expression.ParenthesizedExpression >> node


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
