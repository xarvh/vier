module Solve exposing (..)

import Array exposing (Array)
import CanonicalAst as CA exposing (Name, Pos)
import Constraint exposing (Constraint, Expected)
import Dict exposing (Dict)
import IO
import Type exposing (IO, Type)



-- Stub stuff


type Error
    = Error



----


type alias Env =
    Dict Name Type.Variable


type alias Acc =
    { env : Env
    , mark : Type.Mark
    , errors : List Error
    }


initAcc : Acc
initAcc =
    { env = Dict.empty
    , mark = Type.nextMark Type.noMark
    , errors = []
    }


run : Constraint -> IO (Result (List Error) (Dict Name CA.Annotation))
run constraint =
    IO.do (Type.poolsNew Type.poolsInit) <| \pools ->
    IO.do (solve Dict.empty Type.outermostRank pools constraint initAcc) <| \acc ->
    if acc.errors == [] then
        Debug.todo "Right <$> traverse Type.toAnnotation acc.env"

    else
        acc.errors
            |> Err
            |> IO.return



-- Solver


atToValue : CA.At a -> a
atToValue (CA.At _ a) =
    a


at_traverse : (a -> IO b) -> CA.At a -> IO (CA.At b)
at_traverse func (CA.At pos a) =
    IO.do (func a) <| (CA.At pos >> IO.return)


solve : Env -> Int -> Type.Pools -> Constraint -> Acc -> IO Acc
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
                Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return acc

                Err ( vars, actualType, expectedType ) ->
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
                Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return acc

                Err ( vars, actualType, expectedType ) ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError acc <|
                            Error.BadExpr region (Error.Local name) actualType <|
                                Error.typeReplace expectation expectedType

        Constraint.Foreign region name (CA.Forall freeVars srcType) expectation ->
            IO.do (srcTypeToVariable rank pools freeVars srcType) <| \actual ->
            IO.do (expectedToVariable rank pools expectation) <| \expected ->
            IO.do (Unify.unify actual expected) <| \answer ->
            case answer of
                Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return acc

                Err ( vars, actualType, expectedType ) ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError acc <|
                            Error.BadExpr region (Error.Foreign name) actualType <|
                                Error.typeReplace expectation expectedType

        Constraint.Pattern region category ty expectation ->
            IO.do (typeToVariable rank pools ty) <| \actual ->
            IO.do (patternExpectationToVariable rank pools expectation) <| \expected ->
            IO.do (Unify.unify actual expected) <| \answer ->
            case answer of
                Ok vars ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return acc

                Err ( vars, actualType, expectedType ) ->
                    IO.do (introduce rank pools vars) <| \_ ->
                    IO.return <|
                        addError acc <|
                            Error.BadPattern region
                                category
                                actualType
                                (Error.ptypeReplace expectation expectedType)

        Constraint.And constraints ->
            IO.list_foldl (solve env rank pools) acc constraints

        Constraint.Let params ->
            if params.rigidVars == [] && params.bodyCon == Constraint.True_ then
                IO.do (introduce rank pools params.flexVars) <| \_ ->
                solve env rank pools acc params.headerCon

            else if params.rigidVars == [] && params.flexVars == [] then
                IO.do (solve env rank pools acc params.headerCon) <| \state1 ->
                IO.do (traverse (at_traverse (typeToVariable rank pools)) params.header) <| \locals ->
                let
                    newEnv =
                        Dict.union env (Dict.map atToValue locals)
                in
                IO.do (solve newEnv rank pools state1 params.bodyCon) <| \state2 ->
                IO.list_foldl occurs state2 <| Dict.toList locals

            else
                -- work in the next pool to localize header
                let
                    nextRank =
                        rank + 1

                    poolsLength =
                        Type.poolsLength pools

                    maybeGrowPools =
                        if nextRank < poolsLength then
                            IO.return pools

                        else
                            Type.poolsGrow pools poolsLength
                in
                IO.do maybeGrowPools <| \nextPools ->
                -- introduce variables
                let
                    vars =
                        params.rigidVars ++ params.flexVars

                    doStuff =
                        IO.list_for vars <| \var ->
                        Type.flexModify var <| \descriptor -> { descriptor | rank = nextRank }
                in
                IO.do doStuff <| \_ ->
                IO.do (Type.poolsWrite nextPools nextRank vars) <| \_ ->
                -- run solver in next pool
                IO.do (traverse (at_traverse (typeToVariable nextRank nextPools)) params.header) <| \locals ->
                IO.do (solve env nextRank nextPools acc params.headerCon) <| \accX ->
                -- (Acc savedEnv mark errors) = acc
                let
                    youngMark =
                        accX.mark

                    visitMark =
                        Type.nextMark youngMark

                    finalMark =
                        Type.nextMark visitMark
                in
                -- pop pool
                IO.do (generalize youngMark visitMark nextRank nextPools) <| \_ ->
                IO.do (Type.poolsWrite nextPools nextRank []) <| \_ ->
                -- check that things went well
                -- TODO the Haskell code was using mapM_ instead of forM_.. why?
                IO.do (IO.list_map isGeneric params.rigidVars) <| \_ ->
                let
                    newEnv =
                        Dict.union env (Dict.map atToValue locals)

                    tempState =
                        { accX | mark = finalMark }
                in
                IO.do (solve newEnv rank nextPools tempState params.bodyCon) <| \newState ->
                IO.list_foldl occurs newState (Dict.toList locals)


typeToVariable : Int -> Type.Pools -> Type -> IO Type.Variable
typeToVariable rank pools ty =
    typeToVar rank pools Dict.empty ty


typeToVar : Int -> Type.Pools -> Dict Name Type.Variable -> Type -> IO Type.Variable
typeToVar rank pools aliasDict tipe =
    let
        go =
            typeToVar rank pools aliasDict
    in
    case tipe of
        Type.VarN v ->
            IO.return v

        Type.AppN home name args ->
            IO.do (traverse go args) <| \argVars ->
            register rank pools (Structure (App1 home name argVars))

        Type.FunN a b ->
            IO.do (go a) <| \aVar ->
            IO.do (go b) <| \bVar ->
            register rank pools (Structure (Fun1 aVar bVar))

        Type.AliasN home name args aliasType ->
            IO.do (traverse (traverse go) args) <| \argVars ->
            IO.do (typeToVar rank pools (Dict.fromList argVars) aliasType) <| \aliasVar ->
            register rank pools (Alias home name argVars aliasVar)

        Type.PlaceHolder name ->
            IO.return (aliasDict ! name)

        Type.RecordN fields ext ->
            IO.do (traverse go fields) <| \fieldVars ->
            IO.do (go ext) <| \extVar ->
            register rank pools (Structure (Record1 fieldVars extVar))

        Type.EmptyRecordN ->
            register rank pools emptyRecord1

        Type.UnitN ->
            register rank pools unit1

        Type.TupleN a b c ->
            IO.do (go a) <| \aVar ->
            IO.do (go b) <| \bVar ->
            IO.do (traverse go c) <| \cVar ->
            register rank pools (Structure (Tuple1 aVar bVar cVar))


expectedToVariable : Int -> Type.Pools -> Expected Type -> IO Type.Variable
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


patternExpectationToVariable : Int -> Type.Pools -> Constraint.PatternExpected Type -> IO Type.Variable
patternExpectationToVariable rank pools expectation =
    typeToVariable rank
        pools
        (case expectation of
            Error.PatternExpected_NoExpectation tipe ->
                tipe

            Error.PatternExpected_FromContext _ _ tipe ->
                tipe
        )


introduce : Int -> Type.Pools -> List Type.Variable -> IO ()
introduce rank pools variables =
    IO.do (Type.modifyPools rank ((++) variables) pools) <| \_ ->
    IO.list_for variables <| \var ->
    Type.flexModify var (\descriptor -> { descriptor | rank = rank })


occurs : Acc -> ( Name, CA.At Variable ) -> IO Acc
occurs acc ( name, CA.At pos variable ) =
    Debug.todo "occurs"



-- GENERALIZE


{-|

> Every variable has rank less than or equal to the maxRank of the pool.
> This sorts variables into the young and old pools accordingly.

-}
generalize : Mark -> Mark -> Int -> Pools -> IO ()
generalize youngMark visitMark youngRank pools =
    IO.do (Type.poolsReadAll pools youngRank) <| \youngVars ->
    IO.do (poolToRankTable youngMark youngRank youngVars) <| \rankTable ->
    -- get the ranks right for each entry.
    -- start at low ranks so that we only have to pass
    -- over the information once.
    IO.do (IO.array_indexMap (\rank table -> IO.list_map (adjustRank youngMark visitMark rank) table) rankTable) <| \_ ->
    -- For variables that have rank lowerer than youngRank, register them in
    -- the appropriate old pool if they are not redundant.
    let
        addNonRedundantVars : List Variable -> IO ()
        addNonRedundantVars vars =
            IO.list_for vars <| \var ->
            IO.do (UF.redundant var) <| \isRedundant ->
            if isRedundant then
                IO.return ()

            else
                IO.do (UF.get var) <| \descriptor ->
                poolsModify pools ((::) var) descriptor.rank
    in
    IO.do (IO.array_for (Array.slice 0 -1 rankTable) addNonRedundantVars) <| \_ ->
    -- For variables with rank youngRank
    --   If rank < youngRank: register in oldPool
    --   otherwise generalize
    IO.list_for (array_unsafeLast rankTable) <| \var ->
    IO.do (UF.redundant var) <| \isRedundant ->
    if isRedundant then
        return ()

    else
        IO.do (UF.get var) <| \descriptor ->
        if rank < youngRank then
            poolsModify pools ((::) var) rank

        else
            UF.set var { descriptor | rank = noRank }


array_unsafeLast : Array a -> a
array_unsafeLast arr =
    case Array.get (Array.length arr - 1) arr of
        Nothing ->
            Debug.todo "array_unsafeLast"

        Just a ->
            a


poolToRankTable : Mark -> Int -> List Variable -> IO AllocatedPools
poolToRankTable youngMark youngRank youngInhabitants =
    IO.do (MVector.replicate (youngRank + 1) []) <| \mutableTable ->
    -- Sort the youngPool variables into buckets by rank.
    let
        xxxx var =
            IO.do (UF.get var) <| \descriptor ->
            IO.do (UF.set var { descriptor | mark = youngMark }) <| \_ ->
            IO.do (poolsModify mutableTable ((::) var) rank) <| \_ ->
            IO.return ()
    in
    IO.do (IO.list_for youngInhabitants xxxx) <| \_ ->
    Vector.unsafeFreeze mutableTable
