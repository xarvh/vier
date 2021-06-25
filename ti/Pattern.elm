module Pattern exposing (..)

import CanonicalAst as CA exposing (Name, Pos)
import Constraint exposing (Constraint, Type(..))
import Dict exposing (Dict)
import IO exposing (IO)


type alias Acc =
    { headers : Dict Name (CA.At Constraint.Type)
    , typeVariables : List CA.TyVarRef
    , reversedConstraints : List Constraint
    }


extractType : Constraint.PatternExpected ty -> ty
extractType expectation =
    case expectation of
        Constraint.PatternExpected_NoExpectation ty ->
          ty
        Constraint.PatternExpected_FromContext _ _ ty ->
            ty



--


add : CA.Pattern -> Constraint.PatternExpected Constraint.Type -> Acc -> IO Acc
add (CA.At pos pattern) expectation acc =
    case pattern of
        CA.PatternAnything ->
            IO.return acc

        CA.PatternVariable name ->
            let
                constraint =
                    expectation
                        |> extractType
                        |> CA.At pos
            in
            { add | headers = Dict.insert name constraint acc.headers }
                |> IO.return

        CA.PatternConstructor params ->
            addConstructor pos params expectation acc



{-
   Can.PList patterns ->
     do  entryVar <- mkFlexVar
         let entryType = VarN entryVar
         let listType = AppN ModuleName.list Name.list [entryType]

         (State headers vars revCons) <-
           foldM (addEntry region entryType) state (Index.indexedMap (,) patterns)

         let listCon = CPattern region E.PList listType expectation
         return $ State headers (entryVar:vars) (listCon:revCons)

   Can.PCons headPattern tailPattern ->
     do  entryVar <- mkFlexVar
         let entryType = VarN entryVar
         let listType = AppN ModuleName.list Name.list [entryType]

         let headExpectation = E.PNoExpectation entryType
         let tailExpectation = E.PFromContext region E.PTail listType

         (State headers vars revCons) <-
           add headPattern headExpectation =<<
             add tailPattern tailExpectation state

         let listCon = CPattern region E.PList listType expectation
         return $ State headers (entryVar:vars) (listCon : revCons)

   Can.PRecord fields ->
     do  extVar <- mkFlexVar
         let extType = VarN extVar

         fieldVars <- traverse (\field -> (,) field <$> mkFlexVar) fields
         let fieldTypes = Map.fromList (map (fmap VarN) fieldVars)
         let recordType = RecordN fieldTypes extType

         let (State headers vars revCons) = state
         let recordCon = CPattern region E.PRecord recordType expectation
         return $
           State
             { _headers = Map.union headers (Map.map (A.At region) fieldTypes)
             , _vars = map snd fieldVars ++ extVar : vars
             , _revCons = recordCon : revCons
             }

   Can.PInt _ ->
     do  let (State headers vars revCons) = state
         let intCon = CPattern region E.PInt T.int expectation
         return $ State headers vars (intCon:revCons)

   Can.PStr _ ->
     do  let (State headers vars revCons) = state
         let strCon = CPattern region E.PStr T.string expectation
         return $ State headers vars (strCon:revCons)

   Can.PChr _ ->
     do  let (State headers vars revCons) = state
         let chrCon = CPattern region E.PChr T.char expectation
         return $ State headers vars (chrCon:revCons)

   Can.PBool _ _ ->
     do  let (State headers vars revCons) = state
         let boolCon = CPattern region E.PBool T.bool expectation
         return $ State headers vars (boolCon:revCons)
-}


addConstructor : Pos -> CA.PatternConstructorParams -> Constraint.PatternExpected Type -> Acc -> IO Acc
addConstructor pos params expectation acc0 =
    let
        instanceFlexVar : Name -> IO ( Name, CA.TyVarRef )
        instanceFlexVar unionVarName =
            IO.do (nameToFlex unionVarName) <| \flex ->
            IO.return
                ( unionVarName
                , flex
                )
    in
    IO.do (IO.list_map instanceFlexVar params.union.vars) <| \varPairs ->
    let
        typePairs =
            List.map (Tuple.mapSecond VarN) varPairs

        freeVarDict =
            Dict.fromList typePairs
    in
    IO.do (IO.list_foldl (addCtorArg pos params.ctorName freeVarDict) acc0 params.args) <| \acc1 ->
    let
        constructorType : Type
        constructorType =
            typePairs
                |> List.map Tuple.second
                |> Type.AppN params.moduleName params.typeName

        constructorConstraint : Constraint
        constructorConstraint =
            Constraint.Pattern pos (E.PCtor params.name) constructorType expectation
    in
    IO.return
        { headers = acc1.headers
        , vars = List.map Tuple.second varPairs ++ acc1.vars
        , reversedConstraints = constructorConstraint :: acc1.reversedConstraints
        }


addCtorArg : Pos -> Name -> Dict Name Type -> CA.PatternCtorArg -> Acc -> IO Acc
addCtorArg pos ctorName freeVarDict arg =
    IO.do (Instantiate.fromSrcType freeVarDict arg.srcType) <| \ty ->
    let
        expectation =
            Constraint.PatternExpected_FromContext pos
                (Constraint.PatternContext_ConstructorArg ctorName arg.index)
                ty
    in
    add arg.pattern expectation
