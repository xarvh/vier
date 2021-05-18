module Compiler.ScopeCheck exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Res, errorTodo)
import Types.Meta exposing (Meta)


type alias ReadOnly =
    { meta : Meta

    -- the local names of the module's root values, with their position
    , rootNames : Dict String CA.Pos
    }


{-| The locally defined environment.

We keep ro.rootNames out of it because we need to add dependencies for root names but not local ones.

-}
type alias Env =
    Dict String CA.Pos


type alias Out =
    { dependencies : Dict String (Set String)
    , shadowing : List ( String, ( CA.Pos, CA.Pos ) )
    }


type DoTheShadowingOut
    = ShadowingFound Out
    | EnvUpdated Env


{-| HACK this is just a placeholder
-}
globalPos : CA.Pos
globalPos =
    { n = "--global--"
    , c = ""
    , s = -1
    , e = -1
    }


doTheShadowing : ReadOnly -> CA.Pattern -> Env -> Out -> DoTheShadowingOut
doTheShadowing ro pa env out =
    let
        names =
            CA.patternNames pa

        shadowedGlobals =
            intersectAndPair names ro.meta.globalValues

        shadowedRoot =
            intersectAndPair names ro.rootNames

        shadowedLocals =
            intersectAndPair names env
    in
    if shadowedGlobals /= Dict.empty || shadowedRoot /= Dict.empty || shadowedLocals /= Dict.empty then
        Dict.empty
            |> Dict.union (Dict.map (\k -> Tuple.mapSecond (always globalPos)) shadowedGlobals)
            |> Dict.union shadowedRoot
            |> Dict.union shadowedLocals
            |> Dict.toList
            |> (\new -> { out | shadowing = out.shadowing ++ new })
            |> ShadowingFound

    else
        names
            |> Dict.foldl Dict.insert env
            |> EnvUpdated


doTheDependency ro env pos var out =
    {- TODO dependencies
       * if in env
             do nothing
       * if in ro.rootNames
             add dependency (using fully qualified name)
             (fully qualified name is probably added already by translate)

       * if in ro.meta.globals
             add dependency (using fully qualified name)

       * else
             complain about undefined
    -}
    out



----
--- Crawler
--


onModule : Meta -> CA.AllDefs -> Res (Dict String (Set String))
onModule meta allDefs =
    let
        ( aliases, unions, values ) =
            CA.split allDefs

        names =
            Dict.foldl (\k vdef -> Dict.insert vdef.localName vdef.pos) Dict.empty values

        ro : ReadOnly
        ro =
            { meta = meta
            , rootNames = names
            }

        init =
            { dependencies = Dict.empty
            , shadowing = []
            }

        { dependencies, shadowing } =
            values
                |> Dict.values
                |> List.foldl (\vdef -> onBlock ro Dict.empty vdef.body) init

        -- TODO : check also dependencies and type variables used in aliases and unions
    in
    if shadowing /= [] then
        shadowing
            |> List.map errorShadowing
            |> Error.Nested
            |> Err

    else
        Ok dependencies


errorShadowing : ( String, ( CA.Pos, CA.Pos ) ) -> Error.Error
errorShadowing ( varName, ( second, first ) ) =
    if first == globalPos then
        Error.makeError
            second.n
            [ Error.showLines second.c 2 second.s
            , Error.text <| "the variable name " ++ varName ++ " is used already by a global value"
            ]

    else
        Error.makeError
            second.n
            [ Error.showLines second.c 2 second.s
            , Error.text <| "value " ++ varName ++ " was already declared here: "
            , Error.showLines first.c 2 first.s
            ]


onBlock : ReadOnly -> Env -> List CA.Statement -> Out -> Out
onBlock ro env0 block out0 =
    let
        ( env1, out1 ) =
            List.foldl (onStatement ro) ( env0, out0 ) block
    in
    out1


onStatement : ReadOnly -> CA.Statement -> ( Env, Out ) -> ( Env, Out )
onStatement ro stat ( env, out ) =
    case stat of
        CA.Definition def ->
            case doTheShadowing ro def.pattern env out of
                ShadowingFound out1 ->
                    -- Stop here so that redundant errors won't pile up
                    ( env
                    , out1
                    )

                EnvUpdated env1 ->
                    ( env1
                    , onBlock ro env1 def.body out
                    )

        CA.Evaluation expr ->
            ( env
            , onExpr ro env expr out
            )


onExpr : ReadOnly -> Env -> CA.Expression -> Out -> Out
onExpr ro env expression out =
    case expression of
        CA.Lambda pos param body ->
            let
                pattern =
                    case param of
                        CA.ParameterPattern pa ->
                            pa

                        CA.ParameterMutable po n ->
                            CA.PatternAny po n
            in
            case doTheShadowing ro pattern env out of
                ShadowingFound out1 ->
                    -- Stop here so that redundant errors won't pile up
                    out1

                EnvUpdated env1 ->
                    onBlock ro env1 body out

        CA.Variable pos var ->
            doTheDependency ro env pos var out

        CA.Literal pos value ->
            out

        CA.Record pos ext attrs ->
            let
                out1 =
                    ext
                        |> Maybe.map (\v -> doTheDependency ro env pos v out)
                        |> Maybe.withDefault out

                out2 =
                    Dict.foldl (\k -> onExpr ro env) out1 attrs
            in
            out2

        CA.Call pos expr argument ->
            let
                out1 =
                    onExpr ro env expr out

                out2 =
                    case argument of
                        CA.ArgumentMutable mut ->
                            out1

                        CA.ArgumentExpression e ->
                            onExpr ro env e out1
            in
            out2

        CA.If pos { condition, true, false } ->
            [ condition
            , true
            , false
            ]
                |> List.foldl (onBlock ro env) out

        CA.Try pos expr pas ->
            let
                out1 =
                    onExpr ro env expr out

                doPattern ( pattern, block ) outX =
                    case doTheShadowing ro pattern env outX of
                        ShadowingFound outX1 ->
                            outX1

                        EnvUpdated env1 ->
                            onBlock ro env1 block outX

                out2 =
                    List.foldl doPattern out1 pas
            in
            out2


{-| I assume this function is faster if the 1st dict is smaller than the 2nd one, so I name them accordingly
-}
intersectAndPair : Dict comparable a -> Dict comparable b -> Dict comparable ( a, b )
intersectAndPair small large =
    Dict.foldl
        (\key smallValue acc ->
            case Dict.get key large of
                Nothing ->
                    acc

                Just largeValue ->
                    Dict.insert key ( smallValue, largeValue ) acc
        )
        Dict.empty
        small
