module Compiler.ApplyAliases exposing (..)

{-| The alternative to this module is to make Compiler.TypeInference do it instead,
but at least for now I want to keep that module as light as possible.

Later on, it might be good to solve alias dependencies and maybe unions, then allow
TypeInference to do the replacement of annotations on the fly, which would avoid
allocating another AST.

-}

import Dict exposing (Dict)
import Lib
import RefHierarchy
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Type)
import Types.Error as Error exposing (Res, errorTodo)


type alias Name =
    String


type alias GetAlias =
    Name -> Res (Maybe CA.AliasDef)


replaceType : GetAlias -> Type -> Res Type
replaceType ga ty =
    case ty of
        CA.TypeVariable pos name ->
            Ok ty

        CA.TypeFunction pos from fromIsMutable to ->
            Result.map2
                (\f t -> CA.TypeFunction pos f fromIsMutable t)
                (replaceType ga from)
                (replaceType ga to)

        CA.TypeRecord pos extensible attrs ->
            attrs
                |> Lib.dict_mapRes (\k -> replaceType ga)
                |> Result.map (CA.TypeRecord pos extensible)

        CA.TypeAlias pos path t ->
            -- it's easy to deal with, but it shouldn't happen O_O
            errorTodo "Did we apply aliases twice?"

        CA.TypeConstant pos ref args ->
            Lib.result_do (Lib.list_mapRes (replaceType ga) args) <| \replacedArgs ->
            case ga ref of
                Err e ->
                    Err e

                Ok Nothing ->
                    replacedArgs
                        |> CA.TypeConstant pos ref
                        |> Ok

                Ok (Just al) ->
                    if List.length al.args /= List.length replacedArgs then
                        errorTodo <| "alias " ++ al.name ++ " needs " ++ String.fromInt (List.length al.args) ++ " args, but was used with " ++ String.fromInt (List.length replacedArgs)

                    else
                        let
                            typeByArgName =
                                List.map2 Tuple.pair al.args replacedArgs |> Dict.fromList
                        in
                        al.ty
                            |> expandAliasVariables typeByArgName
                            |> CA.TypeAlias pos ref
                            |> Ok



----
--- Main
--


applyAliasesToModule : CA.AllDefs -> Res CA.AllDefs
applyAliasesToModule mod =
    let
        do =
            Lib.result_do

        ( aliases, unions, values ) =
            CA.split mod
    in
    do (applyAliasesToAliases aliases) <| \resolved_aliases ->
    do (applyAliasesToUnions resolved_aliases unions) <| \resolved_unions ->
    do (applyAliasesToValues resolved_aliases values) <| \resolved_values ->
    let
        a0 =
            Dict.empty

        a1 =
            Dict.foldl (\k v -> Dict.insert k (CA.Alias v)) a0 resolved_aliases

        a2 =
            Dict.foldl (\k v -> Dict.insert k (CA.Union v)) a1 resolved_unions

        a3 =
            Dict.foldl (\k v -> Dict.insert k (CA.Value v)) a2 resolved_values
    in
    Ok a3



----
--- Apply aliases to unions
--


applyAliasesToUnions : Dict Name CA.AliasDef -> Dict Name CA.UnionDef -> Res (Dict Name CA.UnionDef)
applyAliasesToUnions aliases =
    let
        getAlias : GetAlias
        getAlias name =
            Ok <| Dict.get name aliases

        mapConstructor name args =
            Lib.list_mapRes (replaceType getAlias) args

        mapUnion name union =
            Result.map
                (\cs -> { union | constructors = cs })
                (Lib.dict_mapRes mapConstructor union.constructors)
    in
    Lib.dict_mapRes mapUnion



----
--- Apply aliases to annotations
--


type alias ValueDef a =
    { a | maybeAnnotation : Maybe CA.Type, body : List CA.Statement }


applyAliasesToValues : Dict Name CA.AliasDef -> Dict String (ValueDef a) -> Res (Dict String (ValueDef a))
applyAliasesToValues aliases =
    let
        ga : GetAlias
        ga name =
            Dict.get name aliases |> Ok
    in
    Lib.dict_mapRes (\k -> normalizeValueDef ga)


normalizeValueDef : GetAlias -> ValueDef a -> Res (ValueDef a)
normalizeValueDef ga vdef =
    Result.map2
        (\maybeAnnotation body ->
            { vdef
                | maybeAnnotation = maybeAnnotation
                , body = body
            }
        )
        (normalizeAnnotation ga vdef.maybeAnnotation)
        (normalizeBlock ga vdef.body)


normalizeAnnotation : GetAlias -> Maybe Type -> Res (Maybe Type)
normalizeAnnotation ga maybeType =
    case maybeType of
        Nothing ->
            Ok Nothing

        Just ty ->
            replaceType ga ty |> Result.map Just


normalizeBlock : GetAlias -> List CA.Statement -> Res (List CA.Statement)
normalizeBlock ga =
    Lib.list_mapRes (normalizeStatement ga)


normalizeStatement : GetAlias -> CA.Statement -> Res CA.Statement
normalizeStatement ga s =
    case s of
        CA.Definition vdef ->
            Result.map CA.Definition (normalizeValueDef ga vdef)

        CA.Evaluation expr ->
            Result.map CA.Evaluation (normalizeExpr ga expr)


normalizeExpr : GetAlias -> CA.Expression -> Res CA.Expression
normalizeExpr ga expr =
    case expr of
        CA.Literal _ _ ->
            Ok expr

        CA.Variable _ _ ->
            Ok expr

        CA.Lambda pos pattern body0 ->
            body0
                |> normalizeBlock ga
                |> Result.map (CA.Lambda pos pattern)

        CA.Record pos extend attrs0 ->
            attrs0
                |> Lib.dict_mapRes (\k -> normalizeExpr ga)
                |> Result.map (CA.Record pos extend)

        CA.Call pos ref0 arg0 ->
            Result.map2
                (CA.Call pos)
                (normalizeExpr ga ref0)
                (normalizeArg ga arg0)

        CA.If pos ar ->
            Result.map3
                (\c t f -> CA.If pos { condition = c, true = t, false = f })
                (normalizeBlock ga ar.condition)
                (normalizeBlock ga ar.true)
                (normalizeBlock ga ar.false)

        CA.Try pos value tries ->
            Result.map2
                (CA.Try pos)
                (normalizeExpr ga value)
                (Lib.list_mapRes (Lib.tuple_mapSecondRes (normalizeBlock ga)) tries)


normalizeArg : GetAlias -> CA.Argument -> Res CA.Argument
normalizeArg ga arg =
    case arg of
        CA.ArgumentMutable _ ->
            Ok arg

        CA.ArgumentExpression expr ->
            Result.map CA.ArgumentExpression (normalizeExpr ga expr)



----
--- Apply aliases to aliases
--


applyAliasesToAliases : Dict Name CA.AliasDef -> Res (Dict Name CA.AliasDef)
applyAliasesToAliases als =
    let
        orderedAliases =
            als
                |> Dict.values
                |> RefHierarchy.reorder .name findAllRefs_alias
    in
    Lib.list_foldlRes (processAlias als) orderedAliases Dict.empty


processAlias : Dict Name CA.AliasDef -> CA.AliasDef -> Dict Name CA.AliasDef -> Res (Dict Name CA.AliasDef)
processAlias allAliases al processedAliases =
    let
        getAlias name =
            if Dict.member name allAliases then
                case Dict.get name processedAliases of
                    Nothing ->
                        errorTodo "circular!"

                    Just processedAlias ->
                        Ok (Just processedAlias)

            else
                Ok Nothing
    in
    Lib.result_do (replaceType getAlias al.ty) <| \ty ->
    Dict.insert al.name { al | ty = ty } processedAliases
        |> Ok


expandAliasVariables : Dict Name Type -> Type -> Type
expandAliasVariables typeByArgName ty =
    case ty of
        CA.TypeVariable pos name ->
            case Dict.get name typeByArgName of
                Nothing ->
                    ty

                Just t ->
                    t

        CA.TypeFunction pos from fromIsMutable to ->
            CA.TypeFunction pos
                (expandAliasVariables typeByArgName from)
                fromIsMutable
                (expandAliasVariables typeByArgName to)

        CA.TypeRecord pos extensible attrs ->
            CA.TypeRecord pos
                extensible
                (Dict.map (\k -> expandAliasVariables typeByArgName) attrs)

        CA.TypeConstant pos ref args ->
            CA.TypeConstant pos
                ref
                (List.map (expandAliasVariables typeByArgName) args)

        CA.TypeAlias pos ref t ->
            CA.TypeAlias pos ref (expandAliasVariables typeByArgName t)



----
--- Find all refs
--


findAllRefs_alias : CA.AliasDef -> Set String
findAllRefs_alias al =
    findAllRefs_type al.ty


findAllRefs_type : CA.Type -> Set String
findAllRefs_type ty =
    case ty of
        CA.TypeConstant pos ref args ->
            List.foldl (\ar -> Set.union (findAllRefs_type ar)) (Set.singleton ref) args

        CA.TypeVariable pos name ->
            Set.empty

        CA.TypeFunction pos from maybeMut to ->
            Set.union (findAllRefs_type from) (findAllRefs_type to)

        CA.TypeRecord pos extensible attrs ->
            Dict.foldl (\name t -> Set.union (findAllRefs_type t)) Set.empty attrs

        CA.TypeAlias pos path t ->
            findAllRefs_type t
