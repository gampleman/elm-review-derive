module ResolvedType exposing
    ( ResolvedType(..)
    , Reference, fromDeclaration, fromTypeSignature, refToExpr, resolveLocalReferences
    )

{-|

@docs ResolvedType

-}

import CodeGen.Helpers as Helpers
import Dict
import Elm.CodeGen
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TA exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


type alias Reference =
    { modulePath : List String
    , name : String
    }


refToExpr : ModuleName -> List { moduleName : ModuleName, moduleAlias : Maybe String, exposingList : Exposing } -> Reference -> Expression
refToExpr currentModule imports ref =
    if ref.modulePath == currentModule then
        Elm.CodeGen.fun ref.name

    else
        case Helpers.find (\import_ -> import_.moduleName == ref.modulePath) imports of
            Just { moduleAlias, exposingList } ->
                if isExposed exposingList ref.name then
                    Elm.CodeGen.fun ref.name

                else
                    case moduleAlias of
                        Just moduleAlias_ ->
                            Elm.CodeGen.fqFun [ moduleAlias_ ] ref.name

                        Nothing ->
                            Elm.CodeGen.fqFun ref.modulePath ref.name

            Nothing ->
                Debug.todo "This shouldn't happen methinks, otherwise we probably need to insert an import statement"


isExposed : Exposing -> String -> Bool
isExposed exposing_ functionOrValue =
    case exposing_ of
        Elm.Syntax.Exposing.All _ ->
            True

        Elm.Syntax.Exposing.Explicit exposings ->
            List.any
                (\(Node _ a) ->
                    case a of
                        Elm.Syntax.Exposing.InfixExpose _ ->
                            False

                        Elm.Syntax.Exposing.FunctionExpose function ->
                            functionOrValue == function

                        Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAlias ->
                            functionOrValue == typeOrAlias

                        Elm.Syntax.Exposing.TypeExpose typeExpose ->
                            functionOrValue == typeExpose.name
                )
                exposings


{-| Represents an Elm type that has been fully resolved:

  - all references are now in absolute module format
  - type variable applications have been applied
  - custom type and type alias information has been added

The constructors are:

  - `GenericType "foo"` represents an unfilled type variable `foo`
  - `Opaque ref vars` represents a custom type (or built in type)
  - `Function args return` represents a function type
  - `TypeAliasRecord ref args definition` represents a record where we have a type alias (and therefore a constructor) available
  - `AnonymouseRecord definition` represents an anonymous record
  - `CustomType ref args [(constructorRef, arguments)]` represents a reference to an exposed (or accessible) custom type
  - `Tuple args` represents a tuple.

Let's look at some simple examples and see how they would be represented:

    Int --> Opaque { modulePath = ["Basics"], name = "Int" } []

    List Int --> Opaque { modulePath = ["Basics"], name = "List" } [Opaque { modulePath = ["Basics"], name = "Int" } []]

    List a --> Opaque { modulePath = ["Basics"], name = "List" } [ GenericType "a" ]

    Int -> String  --> Function [ Opaque { modulePath = ["Basics"], name = "Int" } [] ] Opaque { modulePath = ["String"], name = "String" } []

    { foo : Int } --> AnonymousRecord [ ( "foo",  Opaque { modulePath = ["Basics"], name = "Int" } [] ) ]

    () -> Tuple []

    ( Int, String ) --> Tuple [ Opaque { modulePath = ["Basics"], name = "Int" } [], Opaque { modulePath = ["String"], name = "String" } [] ]

Now for some more complex examples:

type Foo a =
Bar a

Foo a --> CustomType { modulePath = [], name = "Foo" } [ GenericType "a" ][ ( { modulePath = [], name = "Bar" }, [ GenericType "a" ] ) ]

Foo Int --> CustomType { modulePath = [], name = "Foo" } [ Opaque { modulePath = ["Basics"], name = "Int" } [] ][ ( { modulePath = [], name = "Bar" }, [ Opaque { modulePath = ["Basics"], name = "Int" } [] ] ) ]

Note how in `Foo Int` the `Int` value is replaced in the definition.

    type alias Qux a =
        { foo : a }

    Qux a --> TypeAliasRecord { modulePath = [], name = "Qux" } [ GenericType "a" ] [( "foo", GenericType "a" )]
    Qux Int --> TypeAliasRecord { modulePath = [], name = "Qux" } [ Opaque { modulePath = ["Basics"], name = "Int" } [] ] [( "foo", Opaque { modulePath = ["Basics"], name = "Int" } [] )]

-}
type ResolvedType
    = GenericType String
    | Opaque Reference (List ResolvedType)
      --TODO: | Recursive Reference (List ResolvedType)
    | Function (List ResolvedType) ResolvedType
    | TypeAlias Reference (List String) ResolvedType
    | AnonymousRecord (List ( String, ResolvedType ))
    | CustomType Reference (List String) (List ( Reference, List ResolvedType ))
    | Tuple (List ResolvedType)


fromTypeSignature : ModuleNameLookupTable -> List ResolvedType -> ModuleName -> TypeAnnotation -> ResolvedType
fromTypeSignature lookupTable availableTypes currentModule typeAnnotation =
    case typeAnnotation of
        TA.GenericType var ->
            GenericType var

        TA.Unit ->
            Tuple []

        TA.Typed t args ->
            case ModuleNameLookupTable.moduleNameFor lookupTable t of
                Just [] ->
                    computeApplication (lookupDefinition { modulePath = currentModule, name = Node.value t |> Tuple.second } availableTypes) (List.map (Node.value >> fromTypeSignature lookupTable availableTypes currentModule) args)

                Just moduleName ->
                    computeApplication (lookupDefinition { modulePath = moduleName, name = Node.value t |> Tuple.second } availableTypes) (List.map (Node.value >> fromTypeSignature lookupTable availableTypes currentModule) args)

                Nothing ->
                    Debug.todo "Some error handling?"

        --(normalizeReference lookupTable t) (List.map (\(Node r v) -> Node r (normalizeTypes lookupTable v)) args)
        TA.Tupled args ->
            Tuple (List.map (\(Node _ v) -> fromTypeSignature lookupTable availableTypes currentModule v) args)

        TA.Record def ->
            AnonymousRecord (List.map (\(Node _ ( Node _ k, Node _ v )) -> ( k, fromTypeSignature lookupTable availableTypes currentModule v )) def)

        TA.GenericRecord var (Node defr def) ->
            Debug.todo "Not sure how to handle these yet..."

        TA.FunctionTypeAnnotation (Node _ lv) (Node _ rv) ->
            case fromTypeSignature lookupTable availableTypes currentModule rv of
                Function args ret ->
                    Function (fromTypeSignature lookupTable availableTypes currentModule lv :: args) ret

                ret ->
                    Function [ fromTypeSignature lookupTable availableTypes currentModule lv ] ret


fromDeclaration : ModuleNameLookupTable -> List ResolvedType -> ModuleName -> Declaration -> Maybe ResolvedType
fromDeclaration lookupTable availableTypes currentModule declaration =
    case declaration of
        AliasDeclaration typeAlias ->
            Just
                (TypeAlias
                    { name = Node.value typeAlias.name, modulePath = currentModule }
                    (List.map Node.value typeAlias.generics)
                    (fromTypeSignature lookupTable availableTypes currentModule (Node.value typeAlias.typeAnnotation))
                )

        CustomTypeDeclaration customType ->
            Just
                (CustomType
                    { name = Node.value customType.name, modulePath = currentModule }
                    (List.map Node.value customType.generics)
                    (List.map
                        (\(Node _ constructor) ->
                            ( { name = Node.value constructor.name, modulePath = currentModule }
                            , List.map (Node.value >> fromTypeSignature lookupTable availableTypes currentModule) constructor.arguments
                            )
                        )
                        customType.constructors
                    )
                )

        _ ->
            Nothing


resolveLocalReferences : ModuleName -> List ResolvedType -> List ResolvedType
resolveLocalReferences currentModule types =
    let
        nameDict =
            List.filterMap
                (\t ->
                    case t of
                        Opaque ref _ ->
                            Just ( ref.name, t )

                        TypeAlias ref _ _ ->
                            Just ( ref.name, t )

                        CustomType ref _ _ ->
                            Just ( ref.name, t )

                        _ ->
                            Nothing
                )
                types
                |> Dict.fromList

        traverse t =
            case t of
                GenericType _ ->
                    t

                Opaque ref args ->
                    let
                        newArgs =
                            List.map traverse args
                    in
                    if ref.modulePath == currentModule then
                        case Dict.get ref.name nameDict of
                            Just res ->
                                computeApplication res newArgs

                            Nothing ->
                                Opaque ref newArgs

                    else
                        Opaque ref newArgs

                Function args res ->
                    Function (List.map traverse args) (traverse res)

                TypeAlias ref gens arg ->
                    TypeAlias ref gens (traverse arg)

                AnonymousRecord rec ->
                    AnonymousRecord <|
                        List.map (Tuple.mapSecond traverse) rec

                CustomType ref args ctors ->
                    CustomType ref args (List.map (Tuple.mapSecond (List.map traverse)) ctors)

                Tuple items ->
                    Tuple (List.map traverse items)
    in
    List.map traverse types


lookupDefinition : Reference -> List ResolvedType -> ResolvedType
lookupDefinition reference resolvedTypes =
    case resolvedTypes of
        [] ->
            Opaque reference []

        head :: tail ->
            case head of
                Opaque ref args ->
                    if ref == reference then
                        head

                    else
                        lookupDefinition reference tail

                TypeAlias ref _ _ ->
                    if ref == reference then
                        head

                    else
                        lookupDefinition reference tail

                CustomType ref _ _ ->
                    if ref == reference then
                        head

                    else
                        lookupDefinition reference tail

                _ ->
                    lookupDefinition reference tail


computeApplication : ResolvedType -> List ResolvedType -> ResolvedType
computeApplication applicant arguments =
    case applicant of
        Opaque ref args ->
            Opaque ref (replaceOpaqueArgs args arguments)

        TypeAlias ref args t ->
            let
                bindings =
                    List.map2 Tuple.pair args arguments
                        |> Dict.fromList
            in
            replaceBindings bindings applicant

        CustomType ref args ctors ->
            let
                bindings =
                    List.map2 Tuple.pair args arguments
                        |> Dict.fromList
            in
            replaceBindings bindings applicant

        _ ->
            applicant


replaceBindings : Dict.Dict String ResolvedType -> ResolvedType -> ResolvedType
replaceBindings bindings t =
    case t of
        GenericType name ->
            case Dict.get name bindings of
                Just r ->
                    r

                Nothing ->
                    t

        Opaque ref args ->
            Opaque ref (List.map (replaceBindings bindings) args)

        Function args res ->
            Function (List.map (replaceBindings bindings) args) (replaceBindings bindings res)

        TypeAlias ref varNames child ->
            TypeAlias ref
                (List.filterMap
                    (\varName ->
                        case Dict.get varName bindings of
                            Just (GenericType newName) ->
                                Just newName

                            Nothing ->
                                Just varName

                            _ ->
                                Nothing
                    )
                    varNames
                )
                (replaceBindings bindings child)

        AnonymousRecord rec ->
            AnonymousRecord (List.map (Tuple.mapSecond (replaceBindings bindings)) rec)

        CustomType ref varNames ctors ->
            CustomType ref
                (List.filterMap
                    (\varName ->
                        case Dict.get varName bindings of
                            Just (GenericType newName) ->
                                Just newName

                            Nothing ->
                                Just varName

                            _ ->
                                Nothing
                    )
                    varNames
                )
                (List.map
                    (Tuple.mapSecond (List.map (replaceBindings bindings)))
                    ctors
                )

        Tuple children ->
            Tuple (List.map (replaceBindings bindings) children)


replaceOpaqueArgs : List ResolvedType -> List ResolvedType -> List ResolvedType
replaceOpaqueArgs args vals =
    case args of
        (GenericType _) :: rest ->
            case vals of
                h :: t ->
                    h :: replaceOpaqueArgs rest t

                [] ->
                    args

        somethingElse :: rest ->
            somethingElse :: replaceOpaqueArgs rest vals

        [] ->
            vals
