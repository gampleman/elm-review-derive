module CodeGen.Builtin.Random exposing (generic)

import CodeGen.GenericTodo exposing (Generic)
import Elm.CodeGen as CG
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TA exposing (TypeAnnotation)
import Generator
import ResolvedType
import TypePattern exposing (TypePattern(..))


random =
    { int = \min max -> CG.apply [ CG.fqFun [ "Random" ] "int", min, max ]
    , minInt = CG.fqVal [ "Random" ] "minInt"
    , maxInt = CG.fqVal [ "Random" ] "maxInt"
    , uniform = \fst rest -> CG.apply [ CG.fqFun [ "Random" ] "uniform", fst, rest ]
    , map = \fn arg -> CG.apply [ CG.fqFun [ "Random" ] "map", fn, arg ]
    , map2 = \fn arg1 arg2 -> CG.apply [ CG.fqFun [ "Random" ] "map2", fn, arg1, arg2 ]
    , constant = \arg -> CG.apply [ CG.fqFun [ "Random" ] "constant", arg ]
    , andThen = \arg -> CG.apply [ CG.fqFun [ "Random" ] "andThen", arg ]
    }


randomExtra =
    { anyInt = CG.fqVal [ "Random", "Int" ] "anyInt"
    , andMap = \arg -> CG.apply [ CG.fqVal [ "Random", "Extra" ] "andMap", arg ]
    , choices = \h t -> CG.apply [ CG.fqVal [ "Random", "Extra" ] "choices", h, t ]
    }


generic : Generic
generic =
    Generator.define "elm/random/Random.Generator"
        "elm/random"
        (Typed [ "Random" ] "Generator" [ Target ])
        (\name -> "random" ++ name)
        [ Generator.int (random.int random.minInt random.maxInt)
        , Generator.int randomExtra.anyInt |> Generator.ifUserHasDependency "elm-community/random-extra"
        , Generator.string (random.uniform (CG.string "TODO: Define string options") (CG.list []))
        , Generator.pipeline random.constant (\arg -> CG.apply [ CG.fqFun [ "Random" ] "map2", CG.parens (CG.binOp CG.piper), arg ])
        , Generator.pipeline random.constant randomExtra.andMap |> Generator.ifUserHasDependency "elm-community/random-extra"
        , Generator.mapN 5 (\name ctor args -> CG.apply ([ CG.fqFun [ "Random" ] name, ctor ] ++ args))
        , Generator.map random.map
        , Generator.succeed random.constant
        , Generator.customType
            (\ctors variants ->
                case variants of
                    [] ->
                        Debug.todo "This should never happen"

                    [ ( _, single ) ] ->
                        single

                    ( _, h ) :: t ->
                        CG.pipe (random.uniform h (CG.list (List.map Tuple.second t))) [ random.andThen (CG.val "identity") ]
            )
        , Generator.customType
            (\_ variants ->
                case variants of
                    [] ->
                        Debug.todo "This should never happen"

                    ( _, h ) :: t ->
                        randomExtra.choices h (CG.list (List.map Tuple.second t))
            )
            |> Generator.ifUserHasDependency "elm-community/random-extra"

        -- Random has a weird way to do custom types, which makes the compositional stuff unnatural looking for enums
        , Generator.custom
            (\t ->
                case t of
                    ResolvedType.CustomType _ [] [ ( ref, [] ) ] ->
                        Just (random.constant (CG.fqVal ref.modulePath ref.name))

                    ResolvedType.CustomType _ [] (( head, [] ) :: ctors) ->
                        if List.all (\( _, args ) -> List.isEmpty args) ctors then
                            Just (random.uniform (CG.fqVal head.modulePath head.name) (List.map (\( ctor, _ ) -> CG.fqVal ctor.modulePath ctor.name) ctors |> CG.list))

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        ]
