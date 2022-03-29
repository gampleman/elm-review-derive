module ResolvedType exposing
    ( ResolvedType(..)
    , Reference, fromDeclaration, fromTypeSignature, matchType, refToExpr, resolveLocalReferences
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
                Elm.CodeGen.fqFun ref.modulePath ref.name


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

  - `GenericType "foo" ref` represents an unfilled type variable `foo`
  - `Opaque ref vars` represents a custom type (or built in type)
  - `Function args return` represents a function type
  - `TypeAliasRecord ref args definition` represents a record where we have a type alias (and therefore a constructor) available
  - `AnonymouseRecord extensionVar definition` represents an anonymous record
  - `CustomType ref args [(constructorRef, arguments)]` represents a reference to an exposed (or accessible) custom type
  - `Tuple args` represents a tuple.

Let's look at some simple examples and see how they would be represented:

    Int --> Opaque { modulePath = ["Basics"], name = "Int" } []

    List Int --> Opaque { modulePath = ["Basics"], name = "List" } [Opaque { modulePath = ["Basics"], name = "Int" } []]

    List a --> Opaque { modulePath = ["Basics"], name = "List" } [ GenericType "a" Nothing ]

    Int -> String  --> Function [ Opaque { modulePath = ["Basics"], name = "Int" } [] ] Opaque { modulePath = ["String"], name = "String" } []

    { foo : Int } --> AnonymousRecord Nothing [ ( "foo",  Opaque { modulePath = ["Basics"], name = "Int" } [] ) ]

    { x | foo : Int } --> AnonymousRecord (Just "x") [ ( "foo",  Opaque { modulePath = ["Basics"], name = "Int" } [] ) ]

    () -> Tuple []

    ( Int, String ) --> Tuple [ Opaque { modulePath = ["Basics"], name = "Int" } [], Opaque { modulePath = ["String"], name = "String" } [] ]

Now for some more complex examples:

    type Foo a =
        Bar a

    Foo a --> CustomType { modulePath = [], name = "Foo" } [ "a" ] [ ( { modulePath = [], name = "Bar" }, [ GenericType "a" Nothing ] ) ]

    Foo Int --> CustomType { modulePath = [], name = "Foo" } [ "a" ] [ ( { modulePath = [], name = "Bar" }, [ GenericType "a" (Just (Opaque { modulePath = ["Basics"], name = "Int" } [] ))] ) ]

Note how in `Foo Int` the `Int` value is replaced in the definition.

    type alias Qux a =
        { foo : a }

    Qux a --> TypeAlias { modulePath = [], name = "Qux" } [ "a" ] [( "foo", GenericType "a" Nothing )]
    Qux Int --> TypeAliasRecord { modulePath = [], name = "Qux" } [ "a" ] [( "foo", GenericType "a" (Just (Opaque { modulePath = ["Basics"], name = "Int" } [] )))]

-}
type ResolvedType
    = GenericType String (Maybe ResolvedType)
    | Opaque Reference (List ResolvedType)
    | Function (List ResolvedType) ResolvedType
    | TypeAlias Reference (List String) ResolvedType
    | AnonymousRecord (Maybe String) (List ( String, ResolvedType ))
    | CustomType Reference (List String) (List ( Reference, List ResolvedType ))
    | Tuple (List ResolvedType)


fromTypeSignature : ModuleNameLookupTable -> List ResolvedType -> ModuleName -> TypeAnnotation -> ResolvedType
fromTypeSignature lookupTable availableTypes currentModule typeAnnotation =
    case typeAnnotation of
        TA.GenericType var ->
            GenericType var Nothing

        TA.Unit ->
            Tuple []

        TA.Typed t args ->
            case ModuleNameLookupTable.moduleNameFor lookupTable t of
                Just [] ->
                    computeApplication (lookupDefinition { modulePath = currentModule, name = Node.value t |> Tuple.second } availableTypes) (List.map (Node.value >> fromTypeSignature lookupTable availableTypes currentModule) args)

                Just moduleName ->
                    computeApplication (lookupDefinition { modulePath = moduleName, name = Node.value t |> Tuple.second } availableTypes) (List.map (Node.value >> fromTypeSignature lookupTable availableTypes currentModule) args)

                Nothing ->
                    computeApplication (lookupDefinition { modulePath = currentModule, name = Node.value t |> Tuple.second } availableTypes) (List.map (Node.value >> fromTypeSignature lookupTable availableTypes currentModule) args)

        --(normalizeReference lookupTable t) (List.map (\(Node r v) -> Node r (normalizeTypes lookupTable v)) args)
        TA.Tupled args ->
            Tuple (List.map (\(Node _ v) -> fromTypeSignature lookupTable availableTypes currentModule v) args)

        TA.Record def ->
            AnonymousRecord Nothing (List.map (\(Node _ ( Node _ k, Node _ v )) -> ( k, fromTypeSignature lookupTable availableTypes currentModule v )) def)

        TA.GenericRecord (Node _ var) (Node _ def) ->
            AnonymousRecord (Just var) (List.map (\(Node _ ( Node _ k, Node _ v )) -> ( k, fromTypeSignature lookupTable availableTypes currentModule v )) def)

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


map : (ResolvedType -> ResolvedType) -> ResolvedType -> ResolvedType
map fn t =
    fn
        (case t of
            GenericType n child ->
                GenericType n (Maybe.map (map fn) child)

            Opaque ref args ->
                let
                    newArgs =
                        List.map (map fn) args
                in
                Opaque ref newArgs

            Function args res ->
                Function (List.map (map fn) args) (map fn res)

            TypeAlias ref gens arg ->
                TypeAlias ref gens (map fn arg)

            AnonymousRecord r rec ->
                AnonymousRecord r <|
                    List.map (Tuple.mapSecond (map fn)) rec

            CustomType ref args ctors ->
                CustomType ref args (List.map (Tuple.mapSecond (List.map (map fn))) ctors)

            Tuple items ->
                Tuple (List.map (map fn) items)
        )


resolveLocalReferences : ModuleName -> List ResolvedType -> List ResolvedType
resolveLocalReferences currentModule types =
    let
        getRefKey t =
            case t of
                Opaque ref _ ->
                    Just ref.name

                TypeAlias ref _ _ ->
                    Just ref.name

                CustomType ref _ _ ->
                    Just ref.name

                _ ->
                    Nothing

        initialNameDict =
            List.filterMap
                (\t ->
                    getRefKey t |> Maybe.map (\a -> Tuple.pair a t)
                )
                types
                |> Dict.fromList

        resolveLocalReference t ( names, rest ) =
            let
                result =
                    traverse names t t
            in
            case getRefKey result of
                Just n ->
                    ( Dict.insert n result names, result :: rest )

                Nothing ->
                    ( names, result :: rest )

        cutOfMutualRecursionOverflow parent t =
            case parent of
                CustomType parentRef _ _ ->
                    map
                        (\res ->
                            case res of
                                CustomType ref args _ ->
                                    if ref == parentRef then
                                        Opaque ref (List.map (\a -> GenericType a Nothing) args)

                                    else
                                        res

                                _ ->
                                    res
                        )
                        t

                _ ->
                    t

        traverse nameDict parent =
            map
                (\t ->
                    case t of
                        Opaque ref args ->
                            if ref.modulePath == currentModule && not (matchType parent t) then
                                case Dict.get ref.name nameDict of
                                    Just res ->
                                        computeApplication res args
                                            |> cutOfMutualRecursionOverflow parent

                                    Nothing ->
                                        Opaque ref args

                            else
                                Opaque ref args

                        _ ->
                            t
                )
    in
    List.foldr resolveLocalReference ( initialNameDict, [] ) types |> Tuple.second


logType label t =
    let
        _ =
            Debug.log (label ++ ": " ++ toDebugString t) ""
    in
    t


logTypes label ts =
    let
        _ =
            Debug.log (((label :: List.map toDebugString ts) |> joinWithPadding "- ") ++ "\n") ""
    in
    ts


matchType : ResolvedType -> ResolvedType -> Bool
matchType full possiblyRef =
    case ( full, possiblyRef ) of
        ( CustomType ref1 gens1 _, Opaque ref2 gens2 ) ->
            ref1 == ref2 && List.length gens1 == List.length gens2

        _ ->
            full == possiblyRef


joinWithPadding : String -> List String -> String
joinWithPadding delim =
    List.map (String.lines >> String.join "\n    ")
        >> String.join ("\n    " ++ delim)


toDebugString : ResolvedType -> String
toDebugString t =
    case t of
        GenericType string Nothing ->
            string

        GenericType string (Just child) ->
            string ++ " = " ++ toDebugString child

        Opaque ref children ->
            String.join " " (formatRef ref :: List.map toDebugString children)

        CustomType ref generics ctors ->
            "type "
                ++ formatRef ref
                ++ " "
                ++ String.join " " generics
                ++ "= \n    "
                ++ joinWithPadding "|"
                    (List.map
                        (\( ctorRef, args ) ->
                            formatRef ctorRef ++ " " ++ String.join " " (List.map (toDebugString >> (\arg -> "<" ++ arg ++ ">")) args)
                        )
                        ctors
                    )

        _ ->
            Debug.toString t



-- | Function (List ResolvedType) ResolvedType
-- | TypeAlias Reference (List String) ResolvedType
-- | AnonymousRecord (List ( String, ResolvedType ))
-- |
-- | Tuple (List ResolvedType)


formatRef : Reference -> String
formatRef ref =
    String.join "." (ref.modulePath ++ [ ref.name ])


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
        GenericType name Nothing ->
            case Dict.get name bindings of
                Just (GenericType newName Nothing) ->
                    GenericType newName Nothing

                res ->
                    GenericType name res

        GenericType name (Just child) ->
            case Dict.get name bindings of
                Just (GenericType newName Nothing) ->
                    GenericType newName Nothing

                _ ->
                    GenericType name (Just (replaceBindings bindings child))

        Opaque ref args ->
            Opaque ref (List.map (replaceBindings bindings) args)

        Function args res ->
            Function (List.map (replaceBindings bindings) args) (replaceBindings bindings res)

        -- special case for resolving extensible records
        TypeAlias ref varNames ((AnonymousRecord (Just r) rec) as child) ->
            case Dict.get r bindings of
                Just (AnonymousRecord newR fields) ->
                    AnonymousRecord newR (List.map (Tuple.mapSecond (replaceBindings bindings)) rec ++ fields)

                _ ->
                    TypeAlias ref
                        (List.map
                            (\varName ->
                                case Dict.get varName bindings of
                                    Just (GenericType newName Nothing) ->
                                        newName

                                    _ ->
                                        varName
                            )
                            varNames
                        )
                        (replaceBindings bindings child)

        TypeAlias ref varNames child ->
            TypeAlias ref
                (List.map
                    (\varName ->
                        case Dict.get varName bindings of
                            Just (GenericType newName Nothing) ->
                                newName

                            _ ->
                                varName
                    )
                    varNames
                )
                (replaceBindings bindings child)

        AnonymousRecord r rec ->
            case Maybe.andThen (\var -> Dict.get var bindings) r of
                Just (AnonymousRecord newR fields) ->
                    AnonymousRecord newR (List.map (Tuple.mapSecond (replaceBindings bindings)) rec ++ fields)

                _ ->
                    AnonymousRecord r (List.map (Tuple.mapSecond (replaceBindings bindings)) rec)

        CustomType ref varNames ctors ->
            CustomType ref
                (List.map
                    (\varName ->
                        case Dict.get varName bindings of
                            Just (GenericType newName Nothing) ->
                                newName

                            _ ->
                                varName
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
        (GenericType _ Nothing) :: rest ->
            case vals of
                h :: t ->
                    h :: replaceOpaqueArgs rest t

                [] ->
                    args

        somethingElse :: rest ->
            somethingElse :: replaceOpaqueArgs rest vals

        [] ->
            vals
