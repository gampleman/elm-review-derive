module TodoItForMe exposing (rule)

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Elm.Writer
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructors" initialProjectContext
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
        |> Rule.withDeclarationEnterVisitor declarationVisitor



-- CONTEXT


type alias ModuleNameAsString =
    String


type alias CustomTypeName =
    String


type alias ConstructorName =
    String


type ExposedConstructors
    = ExposedConstructors
        { moduleKey : Rule.ModuleKey
        , customTypes : Dict CustomTypeName (Dict ConstructorName (Node ConstructorName))
        }


type alias ProjectContext =
    { customTypes : List ( ModuleName, Elm.Syntax.Type.Type )
    , typeAliases : List ( ModuleName, TypeAlias )
    , todos : List ( ModuleName, Rule.ModuleKey, Todo )
    }


type alias Todo =
    { functionName : Node String, typeVar : ( ModuleName, String ), declaration : Node Declaration }


type alias ModuleContext =
    { customTypes : List Elm.Syntax.Type.Type
    , typeAliases : List TypeAlias
    , todos : List Todo
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { customTypes = []
    , typeAliases = []
    , todos = []
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    let
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { customTypes = projectContext.customTypes |> List.filter (Tuple.first >> (==) moduleName) |> List.map Tuple.second
    , typeAliases = projectContext.typeAliases |> List.filter (Tuple.first >> (==) moduleName) |> List.map Tuple.second
    , todos =
        List.filterMap
            (\( moduleName_, _, todo ) ->
                if moduleName_ == moduleName then
                    Just todo

                else
                    Nothing
            )
            projectContext.todos
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    let
        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { customTypes = List.map (Tuple.pair moduleName) moduleContext.customTypes
    , typeAliases = List.map (Tuple.pair moduleName) moduleContext.typeAliases
    , todos = List.map (\todo -> ( moduleName, moduleKey, todo )) moduleContext.todos
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypes = newContext.customTypes ++ previousContext.customTypes
    , typeAliases = newContext.typeAliases ++ previousContext.typeAliases
    , todos = newContext.todos ++ previousContext.todos
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


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node_ context =
    case Node.value node_ of
        Declaration.CustomTypeDeclaration customType ->
            ( [], { context | customTypes = customType :: context.customTypes } )

        Declaration.FunctionDeclaration function ->
            ( []
            , { context
                | todos =
                    (case ( function.signature, function.declaration ) of
                        ( Just (Node _ signature), Node _ declaration ) ->
                            case signature.typeAnnotation of
                                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ (TypeAnnotation.Typed (Node _ codecType) []) ]) ->
                                    case declaration.expression of
                                        Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
                                            [ { functionName = signature.name, typeVar = codecType, declaration = node_ } ]

                                        _ ->
                                            []

                                _ ->
                                    []

                        _ ->
                            []
                    )
                        ++ context.todos
              }
            )

        Declaration.AliasDeclaration typeAlias ->
            ( [], { context | typeAliases = typeAlias :: context.typeAliases } )

        _ ->
            ( [], context )


generateRecordCodec : Todo -> Node String -> List (Node ( Node String, Node TypeAnnotation )) -> String
generateRecordCodec todo typeAliasName recordFields =
    List.foldl
        (\(Node _ ( Node _ fieldName, Node _ typeAnnotation )) code ->
            code
                |> pipeLeft
                    (application
                        [ functionOrValue [ "Serialize" ] "field"
                        , Expression.RecordAccessFunction fieldName |> node
                        , case typeAnnotation of
                            TypeAnnotation.Typed (Node _ ( _, typed )) [] ->
                                getCodecName typed

                            _ ->
                                application
                                    [ functionOrValue [ "Debug" ] "todo"
                                    , Expression.Literal "" |> node
                                    ]
                        ]
                    )
        )
        (application
            [ functionOrValue [ "Codec" ] "record"
            , functionOrValue [] (Node.value typeAliasName)
            ]
        )
        recordFields
        |> pipeLeft (application [ functionOrValue [ "Serialize" ] "finishRecord" ])
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write
        |> String.replace "|>" "\n        |>"
        |> (++)
            ((Node.value todo.functionName ++ " : Codec " ++ moduleNameToString todo.typeVar ++ "\n")
                ++ (Node.value todo.functionName ++ " =\n    ")
            )


getCodecName : String -> Node Expression
getCodecName text =
    case text of
        "Int" ->
            functionOrValue [ "Serialize" ] "int"

        "Float" ->
            functionOrValue [ "Serialize" ] "float"

        "String" ->
            functionOrValue [ "Serialize" ] "string"

        _ ->
            functionOrValue [] (uncapitalize text ++ "Codec")


generateCustomTypeCodec : Todo -> Elm.Syntax.Type.Type -> String
generateCustomTypeCodec todo customType =
    let
        args : List ( Node Pattern, ( Node Pattern, Node Expression ) )
        args =
            customType.constructors
                |> List.indexedMap
                    (\index (Node _ constructor) ->
                        let
                            var =
                                index + Char.toCode 'a' |> Char.fromCode |> String.fromChar

                            arguments =
                                List.range 0 (List.length constructor.arguments - 1)
                                    |> List.map (String.fromInt >> (++) "data")
                        in
                        ( VarPattern var |> node
                        , ( NamedPattern
                                { moduleName = [], name = Node.value constructor.name }
                                (List.map (VarPattern >> node) arguments)
                                |> node
                          , application (functionOrValue [] var :: List.map (functionOrValue []) arguments)
                          )
                        )
                    )

        start =
            application
                [ functionOrValue [ "Codec" ] "customType"
                , Expression.LambdaExpression
                    { args = List.map Tuple.first args ++ [ VarPattern "value" |> node ]
                    , expression =
                        Expression.CaseExpression
                            { expression = functionOrValue [] "value", cases = List.map Tuple.second args }
                            |> node
                    }
                    |> node
                    |> Expression.ParenthesizedExpression
                    |> node
                ]
    in
    List.foldl
        (\(Node _ constructor) code ->
            code
                |> pipeLeft
                    (application
                        (functionOrValue [ "Serialize" ] ("variant" ++ String.fromInt (List.length constructor.arguments))
                            :: functionOrValue [] (Node.value constructor.name)
                            :: List.map
                                (\(Node _ typeAnnotation) ->
                                    case typeAnnotation of
                                        TypeAnnotation.Typed (Node _ ( _, typed )) [] ->
                                            getCodecName typed

                                        _ ->
                                            application
                                                [ functionOrValue [ "Debug" ] "todo"
                                                , Expression.Literal "" |> node
                                                ]
                                )
                                constructor.arguments
                        )
                    )
        )
        start
        customType.constructors
        |> pipeLeft (application [ functionOrValue [ "Serialize" ] "finishCustomType" ])
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write
        |> String.replace "|>" "\n        |>"
        |> (++)
            ((Node.value todo.functionName ++ " : Codec " ++ moduleNameToString todo.typeVar ++ "\n")
                ++ (Node.value todo.functionName ++ " =\n    ")
            )


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    projectContext.todos
        |> List.filterMap
            (\( moduleName, moduleKey, todo ) ->
                let
                    localTypeAliases : List TypeAlias
                    localTypeAliases =
                        List.filter (Tuple.first >> (==) moduleName) projectContext.typeAliases
                            |> List.map Tuple.second

                    localCustomTypes : List Elm.Syntax.Type.Type
                    localCustomTypes =
                        List.filter (Tuple.first >> (==) moduleName) projectContext.customTypes
                            |> List.map Tuple.second
                in
                case find (.name >> Node.value >> (==) (Tuple.second todo.typeVar)) localTypeAliases of
                    Just typeAlias ->
                        case typeAlias.typeAnnotation of
                            Node _ (TypeAnnotation.Record fields) ->
                                let
                                    fix : String
                                    fix =
                                        generateRecordCodec todo typeAlias.name fields

                                    range =
                                        Node.range todo.declaration
                                in
                                Rule.errorForModuleWithFix
                                    moduleKey
                                    { message = "Here's my attempt to complete this stub"
                                    , details = [ "" ]
                                    }
                                    range
                                    [ Review.Fix.replaceRangeBy range fix ]
                                    |> Just

                            _ ->
                                Nothing

                    Nothing ->
                        case find (.name >> Node.value >> (==) (Tuple.second todo.typeVar)) localCustomTypes of
                            Just customType ->
                                let
                                    fix : String
                                    fix =
                                        generateCustomTypeCodec todo customType

                                    range =
                                        Node.range todo.declaration
                                in
                                Rule.errorForModuleWithFix
                                    moduleKey
                                    { message = "Here's my attempt to complete this stub"
                                    , details = [ "" ]
                                    }
                                    range
                                    [ Review.Fix.replaceRangeBy range fix ]
                                    |> Just

                            Nothing ->
                                Nothing
            )


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



-- TYPE ANNOTATION UTILITY FUNCTIONS


collectGenericsFromTypeAnnotation : Node TypeAnnotation -> List String
collectGenericsFromTypeAnnotation node_ =
    case Node.value node_ of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectGenericsFromTypeAnnotation a ++ collectGenericsFromTypeAnnotation b

        TypeAnnotation.Typed _ params ->
            List.concatMap collectGenericsFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.GenericRecord _ list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.Tupled list ->
            List.concatMap collectGenericsFromTypeAnnotation list

        TypeAnnotation.GenericType var ->
            [ var ]

        TypeAnnotation.Unit ->
            []


listAtIndex : Int -> List a -> Maybe a
listAtIndex index list =
    case ( index, list ) of
        ( 0, a :: [] ) ->
            Just a

        ( _, [] ) ->
            Nothing

        ( n, _ :: rest ) ->
            listAtIndex (n - 1) rest


uncapitalize : String -> String
uncapitalize text =
    String.toLower (String.left 1 text) ++ String.dropLeft 1 text


moduleNameToString : ( ModuleName, String ) -> String
moduleNameToString ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])
