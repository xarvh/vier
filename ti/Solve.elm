module Solve exposing (..)

import Array exposing (Array)
import CanonicalAst as CA exposing (Name, Pos)
import Constraint exposing (Constraint, Expected)
import Dict exposing (Dict)
import IO exposing (IO)
import Type exposing (Type)



-- Stub stuff


type Error
    = Error



----


type alias Env =
    Dict Name Type.Variable




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
    IO.do (Type.poolsNew Type.poolsInit) <| \pools ->
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
                    IO.return acc

                Unify.Err vars actualType expectedType ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    Error.typeReplace expectation expectedType
                        |> Error.BadExpr region category actualType
                        |> addError acc
                        |> IO.return

        Constraint.Local region name expectation ->
            IO.do (makeCopy rank pools (Dict.get name env)) <| \actual ->
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
                  Type.poolsLength pools

                maybeGrowPools =
                    if nextRank < poolsLength then
                        return pools

                    else
                        Type.poolsGrow pools poolsLength
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


typeToVariable : Int -> Pools -> Type -> IO Type.Variable
typeToVariable rank pools ty =
    typeToVar rank pools Dict.empty ty


typeToVar : Int -> Pools -> Dict Name Type.Variable -> Type -> IO Type.Variable
typeToVar rank pools aliasDict tipe =
    let
        go =
            typeToVar rank pools aliasDict
    in
    case tipe of
        Constraint.VarN v ->
            IO.return v

        Constraint.AppN home name args ->
            IO.do (traverse go args) <| \argVars ->
            register rank pools (Structure (App1 home name argVars))

        Constraint.FunN a b ->
            IO.do (go a) <| \aVar ->
            IO.do (go b) <| \bVar ->
            register rank pools (Structure (Fun1 aVar bVar))

        Constraint.AliasN home name args aliasType ->
            IO.do (traverse (traverse go) args) <| \argVars ->
            IO.do (typeToVar rank pools (Map.fromList argVars) aliasType) <| \aliasVar ->
            register rank pools (Alias home name argVars aliasVar)

        Constraint.PlaceHolder name ->
            IO.return (aliasDict ! name)

        Constraint.RecordN fields ext ->
            IO.do (traverse go fields) <| \fieldVars ->
            IO.do (go ext) <| \extVar ->
            register rank pools (Structure (Record1 fieldVars extVar))

        Constraint.EmptyRecordN ->
            register rank pools emptyRecord1

        Constraint.UnitN ->
            register rank pools unit1

        Constraint.TupleN a b c ->
            IO.do (go a) <| \aVar ->
            IO.do (go b) <| \bVar ->
            IO.do (traverse go c) <| \cVar ->
            register rank pools (Structure (Tuple1 aVar bVar cVar))


expectedToVariable : Int -> Pools -> Expected Type -> IO Type.Variable
expectedToVariable rank pools expectation =
    typeToVariable rank
        pools
        (case expectation of
            Error.NoExpectation tipe ->
                tipe

            Error.FromContext _ _ tipe ->
                tipe

            Error.FromAnnotation _ _ _ tipe ->
                tipe
        )


patternExpectationToVariable : Int -> Pools -> Constraint.PatternExpected Type -> IO Type.Variable
patternExpectationToVariable rank pools expectation =
    typeToVariable rank
        pools
        (case expectation of
            Error.PatternExpected_NoExpectation tipe ->
                tipe

            Error.PatternExpected_FromContext _ _ tipe ->
                tipe
        )


introduce : Int -> Pools -> List Type.Variable -> IO ()
introduce rank pools variables =
    IO.do (modifyPools  rank ((++) variables) pools) <| \_ ->
    forM_ variables <| \var ->
    UF.modify var <| \descriptor ->
    { descriptor | rank = rank }
