module Internal.Builtin.ToString exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define "elm/core/ToString"
        "elm/core"
        (Function Target (Typed [ "String" ] "String" []))
        (\name -> "all" ++ name)
        [ CodeGenerator.custom
            (\t ->
                case t of
                    ResolvedType.CustomType _ _ ctors ->
                        if List.all (\( _, args ) -> List.isEmpty args) ctors then
                            Just
                                (CG.lambda [ CG.varPattern "arg" ]
                                    (CG.caseExpr (CG.val "arg")
                                        (List.map (\( ctor, _ ) -> ( CG.fqNamedPattern ctor.modulePath ctor.name [], CG.string ctor.name )) ctors)
                                    )
                                )

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        ]
