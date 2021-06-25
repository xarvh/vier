module Solve exposing (..)



import Constraint exposing (Constraint)
import IO exposing (IO)
import Dict exposing (Dict)
import Array exposing (Array)
import Type exposing (Type)
import CanonicalAst as CA exposing (Name, Pos)


-- Stub stuff


type Error = Error

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
  , mark =  nextMark noMark
  , errors = []
  }




run : Constraint -> IO (Result (List Error) (Dict Name CA.Annotation))
run constraint =
  IO.do (makePools) <| \pools ->
  IO.do (solve Map.empty outermostRank pools emptyState constraint) <| \acc ->


  if acc.errors == [] then
          Debug.todo "Right <$> traverse Type.toAnnotation acc.env"

  else
     acc.errors
      |> Err
      |> IO.return






-- Solver






solve :: Env -> Int -> Pools -> State -> Constraint -> IO State
solve env rank pools state constraint =
  case constraint of
    CTrue ->
      return state

    CSaveTheEnvironment ->
      return (state { _env = env })

    CEqual region category tipe expectation ->
      do  actual <- typeToVariable rank pools tipe
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region category actualType $
                      Error.typeReplace expectation expectedType

    CLocal region name expectation ->
      do  actual <- makeCopy rank pools (env ! name)
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Local name) actualType $
                      Error.typeReplace expectation expectedType

    CForeign region name (Can.Forall freeVars srcType) expectation ->
      do  actual <- srcTypeToVariable rank pools freeVars srcType
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Foreign name) actualType $
                      Error.typeReplace expectation expectedType

    CPattern region category tipe expectation ->
      do  actual <- typeToVariable rank pools tipe
          expected <- patternExpectationToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadPattern region category actualType
                      (Error.ptypeReplace expectation expectedType)

    CAnd constraints ->
      foldM (solve env rank pools) state constraints

    CLet [] flexs _ headerCon CTrue ->
      do  introduce rank pools flexs
          solve env rank pools state headerCon

    CLet [] [] header headerCon subCon ->
      do  state1 <- solve env rank pools state headerCon
          locals <- traverse (A.traverse (typeToVariable rank pools)) header
          let newEnv = Map.union env (Map.map A.toValue locals)
          state2 <- solve newEnv rank pools state1 subCon
          foldM occurs state2 $ Map.toList locals

    CLet rigids flexs header headerCon subCon ->
      do
          -- work in the next pool to localize header
          let nextRank = rank + 1
          let poolsLength = MVector.length pools
          nextPools <-
            if nextRank < poolsLength
              then return pools
              else MVector.grow pools poolsLength

          -- introduce variables
          let vars = rigids ++ flexs
          forM_ vars $ \var ->
            UF.modify var $ \(Descriptor content _ mark copy) ->
              Descriptor content nextRank mark copy
          MVector.write nextPools nextRank vars

          -- run solver in next pool
          locals <- traverse (A.traverse (typeToVariable nextRank nextPools)) header
          (State savedEnv mark errors) <-
            solve env nextRank nextPools state headerCon

          let youngMark = mark
          let visitMark = nextMark youngMark
          let finalMark = nextMark visitMark

          -- pop pool
          generalize youngMark visitMark nextRank nextPools
          MVector.write nextPools nextRank []

          -- check that things went well
          mapM_ isGeneric rigids

          let newEnv = Map.union env (Map.map A.toValue locals)
          let tempState = State savedEnv finalMark errors
          newState <- solve newEnv rank nextPools tempState subCon

          foldM occurs newState (Map.toList locals)
