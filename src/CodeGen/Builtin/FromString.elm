module CodeGen.Builtin.FromString exposing (..)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


generic : CodeGenerator
generic =
    CodeGenerator.define "elm/core/FromString"
        "elm/core"
        (Function (Typed [ "String" ] "String" []) (Typed [ "Maybe" ] "Maybe" [ Target ]))
        (\name -> "fromString" ++ name)
        [ CodeGenerator.custom
            (\t ->
                case t of
                    ResolvedType.CustomType _ _ ctors ->
                        (List.map
                            (\( ctor, args ) ->
                                ( CG.stringPattern ctor.name
                                , CG.apply [ CG.val "Just", CG.fqConstruct ctor.modulePath ctor.name (List.map (always todoError) args) ]
                                )
                            )
                            ctors
                            ++ [ ( CG.allPattern, CG.val "Nothing" ) ]
                        )
                            |> CG.caseExpr (CG.val "arg")
                            |> CG.lambda [ CG.varPattern "arg" ]
                            |> Just

                    _ ->
                        Nothing
            )
        ]


todoError =
    CG.apply [ CG.fqFun [ "Debug" ] "todo", CG.string "Can't handle this" ]
