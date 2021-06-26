module Solve exposing (..)

import Array exposing (Array)
import CanonicalAst as CA exposing (Name, Pos)
import Constraint exposing (Constraint)
import Dict exposing (Dict)
import IO exposing (IO)
import Type exposing (Type)



-- Stub stuff


type Error
    = Error


makePools : IO Pools
makePools =
    Debug.todo "MVector.replicate 8 []"



----


type alias Env =
    Dict Name Type.Variable


type alias Pools =
    Array (List Type.Variable)


type alias Acc =
    { env : Env
    , mark : Mark
    , errors : List Error
    }


initAcc : Acc
initAcc =
    { env = Dict.empty
    , mark = nextMark noMark
    , errors = []
    }


run : Constraint -> IO (Result (List Error) (Dict Name CA.Annotation))
run constraint =
    IO.do makePools <| \pools ->
    IO.do (solve Map.empty outermostRank pools constraint initAcc) <| \acc ->
    if acc.errors == [] then
        Debug.todo "Right <$> traverse Type.toAnnotation acc.env"

    else
        acc.errors
            |> Err
            |> IO.return



-- Solver


solve : Env -> Int -> Pools -> Constraint -> Acc -> IO Acc
solve env rank pools constraint acc =
    case constraint of
        Constraint.True_ ->
            IO.return acc

        {-
           Constraint.SaveTheEnvironment ->
             IO.return {acc | env = env }
        -}
        Constraint.Equal pos category ty expectation ->
            IO.do (typeToVariable rank pools ty) <| \actual ->
            IO.do (expectedToVariable rank pools expectation) <| \expected ->
            IO.do (Unify.unify actual expected) <| \answer ->
            case answer of
                Unify.Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return state

                Unify.Err vars actualType expectedType ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError state <|
                            Error.BadExpr region category actualType <|
                                Error.typeReplace expectation expectedType

        Constraint.Local region name expectation ->
            IO.do (makeCopy rank pools (env ! name)) <| \actual ->
            IO.do (expectedToVariable rank pools expectation) <| \expected ->
            IO.do (Unify.unify actual expected) <| \answer ->
            case answer of
                Unify.Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return state

                Unify.Err vars actualType expectedType ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError state <|
                            Error.BadExpr region (Error.Local name) actualType <|
                                Error.typeReplace expectation expectedType

        Constraint.Foreign region name (CA.Forall freeVars srcType) expectation ->
            IO.do (srcTypeToVariable rank pools freeVars srcType) <| \actual ->
            IO.do (expectedToVariable rank pools expectation) <| \expected ->
            IO.do (Unify.unify actual expected) <| \answer ->
            case answer of
                Unify.Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return state

                Unify.Err vars actualType expectedType ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError state <|
                            Error.BadExpr region (Error.Foreign name) actualType <|
                                Error.typeReplace expectation expectedType

        Constraint.Pattern region category ty expectation ->
            IO.do (typeToVariable rank pools ty) <| \actual ->
            IO.do (patternExpectationToVariable rank pools expectation) <| \expected ->
            IO.do (Unify.unify actual expected) <| \answer ->
            case answer of
                Unify.Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return state

                Unify.Err vars actualType expectedType ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError state <|
                            Error.BadPattern region
                                category
                                actualType
                                (Error.ptypeReplace expectation expectedType)

        Constraint.And constraints ->
            foldM (solve env rank pools) state constraints

        Constraint.Let [] flexs _ headerCon Constraint.True_ ->
            IO.do (introduce rank pools flexs) <| \_ ->
            solve env rank pools state headerCon

        Constraint.Let [] [] header headerCon subCon ->
            IO.do (solve env rank pools state headerCon) <| \state1 ->
            IO.do (traverse (A.traverse (typeToVariable rank pools)) header) <| \locals ->
            let
                newEnv =
                    Map.union env (Map.map A.toValue locals)
            in
            IO.do (solve newEnv rank pools state1 subCon) <| \state2 ->
            foldM occurs state2 <| Map.toList locals

        Constraint.Let rigids flexs header headerCon subCon ->
            -- work in the next pool to localize header
            let
                nextRank =
                    rank + 1

                poolsLength =
                    MVector.length pools

                maybeGrowPools =
                    if nextRank < poolsLength then
                        return pools

                    else
                        MVector.grow pools poolsLength
            in
            IO.do maybeGrowPools <| \nextPools ->
            -- introduce variables
            let
                vars =
                    rigids ++ flexs

                doStuff =
                    forM_ vars <| \var ->
                    UF.modify var <| \(Descriptor content _ mark copy) ->
                    Descriptor content nextRank mark copy
            in
            IO.do doStuff <| \_ ->
            IO.do (MVector.write nextPools nextRank vars) <| \_ ->
            -- run solver in next pool
            IO.do (traverse (A.traverse (typeToVariable nextRank nextPools)) header) <| \locals ->
            IO.do (solve env nextRank nextPools state headerCon) <| \acc ->
            -- (Acc savedEnv mark errors) = acc
            let
                youngMark =
                    mark

                visitMark =
                    nextMark youngMark

                finalMark =
                    nextMark visitMark
            in
            -- pop pool
            IO.do (generalize youngMark visitMark nextRank nextPools) <| \_ ->
            IO.do (MVector.write nextPools nextRank []) <| \_ ->
            -- check that things went well
            IO.do (mapM_ isGeneric rigids) <| \_ ->
            let
                newEnv =
                    Map.union env (Map.map A.toValue locals)

                tempState =
                    Acc savedEnv finalMark errors
            in
            IO.do (solve newEnv rank nextPools tempState subCon) <| \newState ->
            foldM occurs newState (Map.toList locals)
