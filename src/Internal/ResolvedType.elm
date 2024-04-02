module Internal.ResolvedType exposing (computeVisibility, findGenericAssignments, fromDeclaration, fromTypeSignature, getArgs, getRef, matchType, refToExpr, resolveLocalReferences)

import AssocList
import Dict
import Elm.CodeGen
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TA exposing (TypeAnnotation)
import List.Extra
import ResolvedType exposing (Reference, ResolvedType(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


refToExpr : ModuleName -> List { moduleName : ModuleName, moduleAlias : Maybe String, exposingList : Exposing } -> Reference -> Expression
refToExpr currentModule imports ref =
    if ref.modulePath == currentModule then
        Elm.CodeGen.fun ref.name

    else
        case List.Extra.find (\import_ -> import_.moduleName == ref.modulePath) imports of
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


matchType : ResolvedType -> ResolvedType -> Bool
matchType full possiblyRef =
    case ( full, possiblyRef ) of
        ( CustomType ref1 gens1 _, Opaque ref2 gens2 ) ->
            ref1 == ref2 && List.length gens1 == List.length gens2

        ( Opaque ref2 gens2, CustomType ref1 gens1 _ ) ->
            ref1 == ref2 && List.length gens1 == List.length gens2

        ( TypeAlias ref1 gens1 _, TypeAlias ref2 gens2 _ ) ->
            ref1 == ref2 && List.length gens1 == List.length gens2

        ( Opaque ref1 gens1, Opaque ref2 gens2 ) ->
            ref1
                == ref2
                && List.length gens1
                == List.length gens2
                && List.all identity
                    (List.map2
                        (\gen1 gen2 ->
                            case gen1 of
                                GenericType _ Nothing ->
                                    True

                                _ ->
                                    gen1 == gen2
                        )
                        gens1
                        gens2
                    )

        ( CustomType ref1 gens1 ctors1, CustomType ref2 gens2 ctors2 ) ->
            if full == possiblyRef then
                True

            else if ref1 == ref2 then
                let
                    bindings =
                        List.map2 Tuple.pair gens1 gens2
                            |> Dict.fromList

                    slots2 =
                        getFilledGenericSlots possiblyRef
                            |> Dict.fromList
                in
                getFilledGenericSlots full
                    |> List.all
                        (\( name, t1 ) ->
                            Dict.get name bindings
                                |> Maybe.andThen
                                    (\newName ->
                                        Dict.get newName slots2
                                    )
                                |> Maybe.map
                                    (\t2 ->
                                        matchType t1 t2
                                    )
                                |> Maybe.withDefault False
                        )

            else
                False

        _ ->
            full == possiblyRef


{-| Returns a dictionary of type variable => occupied type
-}
findGenericAssignments : ResolvedType -> ResolvedType -> Dict.Dict String ResolvedType
findGenericAssignments full possiblyRef =
    case ( full, possiblyRef ) of
        ( CustomType _ gens1 _, Opaque _ gens2 ) ->
            List.map2 Tuple.pair gens1 gens2 |> Dict.fromList

        ( Opaque _ gens1, CustomType _ names ctors ) ->
            let
                bindings =
                    List.map2 Tuple.pair names gens1
                        |> List.filterMap
                            (\( name, gen ) ->
                                case gen of
                                    GenericType newName Nothing ->
                                        Just ( name, newName )

                                    _ ->
                                        Nothing
                            )
                        |> Dict.fromList
            in
            getFilledGenericSlots possiblyRef
                |> List.filterMap
                    (\( name, t ) ->
                        Dict.get name bindings
                            |> Maybe.map (\newName -> ( newName, t ))
                    )
                |> Dict.fromList

        ( CustomType ref1 gens1 ctors1, CustomType ref2 gens2 ctors2 ) ->
            let
                bindings =
                    List.map2 Tuple.pair gens2 gens1
                        |> Dict.fromList
            in
            getFilledGenericSlots possiblyRef
                |> List.filterMap
                    (\( name, t1 ) ->
                        Dict.get name bindings
                            |> Maybe.map
                                (\newName ->
                                    ( newName
                                    , t1
                                    )
                                )
                    )
                |> Dict.fromList

        ( TypeAlias _ gens1 _, TypeAlias ref2 gens2 struct ) ->
            let
                bindings =
                    List.map2 Tuple.pair gens1 gens2 |> Dict.fromList
            in
            getFilledGenericSlots struct
                |> List.filterMap
                    (\( name, t ) ->
                        Dict.get name bindings
                            |> Maybe.map (\newName -> ( newName, t ))
                    )
                |> Dict.fromList

        ( Opaque _ gens1, Opaque _ gens2 ) ->
            List.map2
                (\gen1 gen2 ->
                    case gen1 of
                        GenericType name Nothing ->
                            case gen2 of
                                GenericType _ (Just t) ->
                                    Just ( name, t )

                                GenericType _ Nothing ->
                                    Nothing

                                _ ->
                                    Just ( name, gen2 )

                        _ ->
                            Nothing
                )
                gens1
                gens2
                |> List.filterMap identity
                |> Dict.fromList

        _ ->
            Dict.empty


getFilledGenericSlots : ResolvedType -> List ( String, ResolvedType )
getFilledGenericSlots t =
    getGenericSlots t |> List.filterMap (\( n, mt ) -> Maybe.map (\tt -> ( n, tt )) mt)


getArgs : ResolvedType -> List ResolvedType
getArgs t =
    case t of
        Opaque _ args ->
            args

        TypeAlias _ _ arg ->
            getGenericSlots arg
                |> List.map (\( n, tt ) -> GenericType n tt)

        CustomType _ _ ctors ->
            List.concatMap (Tuple.second >> List.concatMap getGenericSlots) ctors
                |> List.map (\( n, tt ) -> GenericType n tt)

        _ ->
            []


getGenericSlots : ResolvedType -> List ( String, Maybe ResolvedType )
getGenericSlots t =
    case t of
        GenericType n child ->
            [ ( n, child ) ]

        Opaque _ args ->
            List.concatMap getGenericSlots args

        Function args res ->
            List.concatMap getGenericSlots args ++ getGenericSlots res

        TypeAlias _ _ arg ->
            getGenericSlots arg

        AnonymousRecord _ rec ->
            List.concatMap (Tuple.second >> getGenericSlots) rec

        CustomType _ _ ctors ->
            List.concatMap (Tuple.second >> List.concatMap getGenericSlots) ctors

        Tuple items ->
            List.concatMap getGenericSlots items



{-
         toString : ResolvedType -> (String, String)
         toString resolvedType =
             let
                 list =
                     List.foldr
                         (\child ( strs, defs ) ->
                             let
                                 ( str, def ) =
                                     helper child
                             in
                             ( str :: strs, def ++ defs )
                         )
                         ( [], [] )

                 helper t =
                     case t of
                         GenericType varName Nothing ->
                             ( varName, [] )

                         GenericType _ (Just child) ->
                             helper child

                         Opaque ref children ->
                             list children
                                 |> Tuple.mapFirst (\r -> String.join " " (formatRef ref :: r))

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
             in
             helper resolvedType




      formatRef : Reference -> String
      formatRef ref =
          String.join "." (ref.modulePath ++ [ ref.name ])

   joinWithPadding : String -> List String -> String
   joinWithPadding delim =
       List.map (String.lines >> String.join "\n    ")
           >> String.join ("\n    " ++ delim)

-}


lookupDefinition : Reference -> List ResolvedType -> ResolvedType
lookupDefinition reference resolvedTypes =
    case resolvedTypes of
        [] ->
            Opaque reference []

        head :: tail ->
            case head of
                Opaque ref _ ->
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

        TypeAlias _ args _ ->
            let
                bindings =
                    List.map2 Tuple.pair args arguments
                        |> Dict.fromList
            in
            replaceBindings bindings applicant

        CustomType _ args _ ->
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


{-| Modifies a type to reflect export declarations. This will typically turn things into `Opaque` values.
-}
computeVisibility : ModuleName -> AssocList.Dict ModuleName (List ( String, Bool )) -> ResolvedType -> ResolvedType
computeVisibility currentModule exports =
    map
        (\t ->
            case t of
                CustomType ref gens _ ->
                    if ref.modulePath == currentModule then
                        t

                    else
                        case AssocList.get ref.modulePath exports of
                            Just [] ->
                                -- this is a `exposing (..)` module
                                t

                            Just exps ->
                                if List.member ( ref.name, True ) exps then
                                    t

                                else
                                    let
                                        bindings =
                                            getFilledGenericSlots t
                                                |> Dict.fromList
                                    in
                                    Opaque ref (List.filterMap (\n -> Dict.get n bindings) gens)

                            Nothing ->
                                -- I don't know? How do we know anything about this type in this situation?
                                t

                _ ->
                    t
        )


getRef : ResolvedType -> Maybe Reference
getRef type_ =
    case type_ of
        Opaque ref _ ->
            Just ref

        TypeAlias ref _ _ ->
            Just ref

        CustomType ref _ _ ->
            Just ref

        _ ->
            Nothing
