module GetConstraints exposing (..)

-- TODO Rename to Constraints.Expressions?

import CanonicalAst as CA exposing (Name, Pos)
import Constraint exposing (Constraint, Expected)
import Dict exposing (Dict)
import IO exposing (IO)
import Pattern
import Type exposing (Type)


type alias RigidTypeVars =
    Dict Name Type


constrain : RigidTypeVars -> CA.Expression -> Expected Type -> IO Constraint
constrain rtv (CA.At pos expression) expected =
    case expression of
        CA.Variable args ->
            case args.annotation of
                Just annotation ->
                    IO.return <| Constraint.Foreign pos args.name annotation expected

                Nothing ->
                    IO.return <| Constraint.Local pos args.name expected

        CA.Constructor moduleName name annotation ->
            IO.return <| Constraint.Foreign pos name annotation expected

        CA.Literal literal ->
            IO.return <| Constraint.Equal pos (literalCategory literal) (literalType literal) expected

        CA.List elements ->
            constrainList rtv pos elements expected

        CA.Lambda args body ->
            constrainLambda rtv pos args body expected

        CA.Call func args ->
            constrainCall rtv pos func args expected

        CA.If branches finally ->
            constrainIf rtv pos branches finally expected

        CA.Try expr branches ->
            constrainCase rtv pos expr branches expected

        CA.Let def body ->
            IO.do (constrain rtv body expected) <| constrainDef rtv def

        --     CA.Update name expr fields ->
        --       constrainUpdate rtv pos name expr fields expected
        CA.Record fields ->
            constrainRecord rtv pos fields expected


literalCategory : CA.Literal -> Constraint.Category
literalCategory lit =
    Constraint.Category_Literal


literalType : CA.Literal -> Type
literalType lit =
    Type.AppN "SPCore/String" "String" []


constrainList : RigidTypeVars -> Pos -> List CA.Expression -> Expected Type -> IO Constraint
constrainList rtv pos items expected =
    IO.do Type.mkFlexVar <| \itemVar ->
    let
        itemType =
            Type.VarN itemVar

        listType =
            Type.AppN "SPCore/List" "List" [ itemType ]
    in
    IO.do (IO.list_indexMap (constrainListEntry rtv pos itemType) items) <| \itemCons ->
    [ Constraint.And itemCons
    , Constraint.Equal pos Constraint.Category_List listType expected
    ]
        |> Constraint.And
        |> Constraint.exists [ itemVar ]
        |> IO.return


constrainListEntry : RigidTypeVars -> Pos -> Type -> Int -> CA.Expression -> IO Constraint
constrainListEntry rtv pos tipe index expr =
    constrain rtv expr (Constraint.Expected_FromContext pos (Constraint.Context_ListEntry index) tipe)


constrainLambda : RigidTypeVars -> Pos -> List CA.Pattern -> CA.Expression -> Expected Type -> IO Constraint
constrainLambda rtv pos args body expected =
    IO.do (constrainArguments args) <| \argsAcc ->
    IO.do (constrain rtv body (Constraint.Expected_NoExpectation argsAcc.result)) <| \bodyCon ->
    [ Constraint.Let
        { rigidVars = []
        , flexVars = argsAcc.patternAcc.typeVariables
        , header = argsAcc.patternAcc.headers
        , headerCon = Constraint.And (List.reverse argsAcc.patternAcc.reversedConstraints)
        , bodyCon = bodyCon
        }
    , Constraint.Equal pos Constraint.Category_Lambda argsAcc.ty expected
    ]
        |> Constraint.And
        |> Constraint.exists argsAcc.vars
        |> IO.return


type alias ArgsAcc =
    { vars : List Type.Variable
    , ty : Type
    , result : Type
    , patternAcc : Pattern.Acc
    }


constrainArguments : List CA.Pattern -> IO ArgsAcc
constrainArguments args =
    constrainArgumentsRec args Pattern.initAcc


constrainArgumentsRec : List CA.Pattern -> Pattern.Acc -> IO ArgsAcc
constrainArgumentsRec args acc =
    case args of
        [] ->
            IO.do Type.mkFlexVar <| \resultVar ->
            let
                resultType =
                    Type.VarN resultVar
            in
            IO.return
                { vars = [ resultVar ]
                , ty = resultType
                , result = resultType
                , patternAcc = acc
                }

        pattern :: otherArgs ->
            IO.do Type.mkFlexVar <| \argVar ->
            let
                argType =
                    Type.VarN argVar
            in
            IO.do (Pattern.add pattern (Constraint.PatternExpected_NoExpectation argType) acc) <| \updatedState ->
            IO.do (constrainArgumentsRec otherArgs updatedState) <| \nextArgs ->
            IO.return
                { nextArgs
                    | vars = argVar :: nextArgs.vars
                    , ty = Type.FunN argType nextArgs.ty
                }


constrainRecord : RigidTypeVars -> Pos -> Dict Name CA.Expression -> Expected Type -> IO Constraint
constrainRecord rtv pos fields expected =
    IO.do (IO.dict_map (\name -> constrainRecordAttribute rtv) fields) <| \dict ->
    let
        recordType =
            Type.RecordN (Dict.map (always .ty) dict) Type.EmptyRecordN

        recordCon =
            Constraint.Equal pos Constraint.Category_Record recordType expected

        vars =
            Dict.foldr (\k r vs -> r.var :: vs) [] dict

        cons =
            Dict.foldr (\k r cs -> r.constraint :: cs) [ recordCon ] dict
    in
    cons
        |> Constraint.And
        |> Constraint.exists vars
        |> IO.return


constrainRecordAttribute : RigidTypeVars -> CA.Expression -> IO { var : Type.Variable, ty : Type, constraint : Constraint }
constrainRecordAttribute rtv expression =
    IO.do Type.mkFlexVar <| \var ->
    let
        ty =
            Type.VarN var
    in
    IO.do (constrain rtv expression (Constraint.Expected_NoExpectation ty)) <| \con ->
    IO.return { var = var, ty = ty, constraint = con }


constrainDef rtv def =
    Debug.todo ""


constrainCall rtv pos func args expected =
    Debug.todo ""


constrainIf rtv pos branches finally expected =
    Debug.todo ""


constrainCase rtv pos expr branches expected =
    Debug.todo ""
