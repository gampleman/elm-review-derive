module Internal.Builtin.FromString exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define
        { id = "elm/core/FromString"
        , dependency = "elm/core"
        , typePattern = Function (Typed [ "String" ] "String" []) (Typed [ "Maybe" ] "Maybe" [ Target ])
        , makeName = \name -> "fromString" ++ name
        }
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
