module CodeGen.Builtin.Codec exposing (..)

import CodeGen.GenericTodo exposing (Generic)
import Elm.CodeGen as CG
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TA exposing (TypeAnnotation)
import Generator
import ResolvedType
import TypePattern exposing (TypePattern(..))


val =
    CG.fqVal [ "Serialize" ]


fn1 name arg =
    CG.apply [ CG.fqVal [ "Serialize" ] name, arg ]


toValueCase : String -> String
toValueCase v =
    String.uncons v
        |> Maybe.map (\( char, res ) -> String.cons (Char.toLocaleLower char) res)
        |> Maybe.withDefault v


generic : Generic
generic =
    Generator.define "MartinSStewart/elm-serialize/Codec"
        "MartinSStewart/elm-serialize"
        (Typed [ "Serialize" ] "Codec" [ GenericType "e", Target ])
        (\name -> toValueCase name ++ "Codec")
        [ Generator.int (val "int")
        , Generator.float (val "float")
        , Generator.string (val "string")
        , Generator.list (fn1 "list")
        , Generator.maybe (fn1 "maybe")
        , Generator.dict (\key value -> CG.apply [ val "dict", key, value ])
        , Generator.unit (val "unit")
        , Generator.tuple (\arg1 arg2 -> CG.apply [ val "tuple", arg1, arg2 ])
        , Generator.triple (\arg1 arg2 arg3 -> CG.apply [ val "tuple", arg1, arg2, arg3 ])
        , Generator.customType
            (\ctors exprs ->
                CG.pipe
                    (CG.apply
                        [ val "customType"
                        , CG.lambda (List.map (\( ctorRef, _ ) -> CG.varPattern (toValueCase ctorRef.name ++ "Encoder")) ctors ++ [ CG.varPattern "value" ])
                            (ctors
                                |> List.map
                                    (\( ctorRef, arguments ) ->
                                        ( CG.fqNamedPattern ctorRef.modulePath ctorRef.name (thingsToPatterns arguments)
                                        , CG.apply (CG.val (toValueCase ctorRef.name ++ "Encoder") :: thingsToValues arguments)
                                        )
                                    )
                                |> CG.caseExpr (CG.val "value")
                            )
                        ]
                    )
                    (List.map Tuple.second exprs ++ [ val "finishCustomType" ])
            )
        , Generator.combiner
            (\t fn exprs ->
                case t of
                    ResolvedType.Opaque ref args ->
                        Just <| CG.apply ([ val ("variant" ++ String.fromInt (List.length args)), fn ] ++ exprs)

                    ResolvedType.AnonymousRecord fields ->
                        Just <| CG.pipe (CG.apply [ val "record", fn ]) (List.map2 (\( field, _ ) expr -> CG.apply [ val "field", CG.accessFun ("." ++ field), expr ]) fields exprs ++ [ val "finishRecord" ])

                    ResolvedType.TypeAlias _ _ (ResolvedType.AnonymousRecord fields) ->
                        Just <| CG.pipe (CG.apply [ val "record", fn ]) (List.map2 (\( field, _ ) expr -> CG.apply [ val "field", CG.accessFun ("." ++ field), expr ]) fields exprs ++ [ val "finishRecord" ])

                    _ ->
                        Nothing
            )
        ]


thingsToPatterns : List a -> List CG.Pattern
thingsToPatterns =
    List.indexedMap (\i _ -> CG.varPattern ("arg" ++ String.fromInt i))


thingsToValues : List a -> List CG.Expression
thingsToValues =
    List.indexedMap (\i _ -> CG.val ("arg" ++ String.fromInt i))
