module Internal.Builtin.ListAllVariants exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define "elm/core/ListAllGenerics"
        "elm/core"
        (Typed [ "List" ] "List" [ Target ])
        (\name -> "all" ++ name)
        [ CodeGenerator.custom
            (\t ->
                case t of
                    ResolvedType.CustomType _ _ ctors ->
                        if List.all (\( _, args ) -> List.isEmpty args) ctors then
                            Just (CG.list (List.map (\( ctor, _ ) -> CG.fqVal ctor.modulePath ctor.name) ctors))

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        ]
