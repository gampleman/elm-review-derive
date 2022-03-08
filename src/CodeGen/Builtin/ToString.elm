module CodeGen.Builtin.ToString exposing (generic)

import CodeGen.GenericTodo exposing (Generic)
import Elm.CodeGen as CG
import Generator
import ResolvedType
import TypePattern exposing (TypePattern(..))


generic : Generic
generic =
    Generator.define "elm/core/ToString"
        "elm/core"
        (Function Target (Typed [ "String" ] "String" []))
        (\name -> "all" ++ name)
        [ Generator.custom
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
