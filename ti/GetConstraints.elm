module GetConstraints exposing (..)

import CanonicalAst as CA exposing (Pos, Name)
import Constraint exposing (Constraint, Expected)
import Type exposing (Type)
import Dict exposing (Dict)
import IO exposing (IO)
import Pattern



-- fold types


type alias ArgsAcc =
    { vars : List Type.Variable
    , ty : Type
    , result : Type
    , patternAcc : Pattern.Acc
    }



--
type alias RigidTypeVars = Dict Name Type

constrain : RigidTypeVars -> CA.Expression -> Expected Type -> IO.Constraint
constrain rtv (CA.At pos expression) expected =
    case expression of
        CA.Variable args ->
            case args.annotation of
                Just annotation ->
                    IO.return <| Constraint.Foreign pos name annotation expected

                Nothing ->
                    IO.return <| Constraint.Local pos name expected

        CA.Constructor moduleName name annotation ->
            IO.return <| Constraint.Foreign pos name annotation expected

        CA.Literal literal ->
            IO.return <| Constraint.Equal pos (literalCategory literal) (literalType literal) expected

        CA.List elements ->
            constrainList rtv pos elements expected

        CA.Lambda args body ->
            constrainLambda rtv pos args body expected

        CA.Call func args ->
            constrainCall rtv region func args expected

        CA.If branches finally ->
            constrainIf rtv pos branches finally expected

        CA.Try expr branches ->
            constrainCase rtv region expr branches expected

        CA.Let def body ->
            constrainDef rtv def =<< constrain rtv body expected

        --     CA.Update name expr fields ->
        --       constrainUpdate rtv region name expr fields expected
        CA.Record fields ->
            constrainRecord rtv region fields expected


literalCategory : CA.Literal -> Constraint.Category
literalCategory lit =
    Constraint.CategoryLiteral


literalType : CA.Literal -> Type
literalType lit =
    CA.TypeConstant () "String" []


constrainList : RigidTypeVars -> Pos -> List Expression -> Type -> IO Constraint
constrainList rtv pos items expected =
    IO.do mkFlexVar <| \itemVar ->
    let
        itemType =
            VarN itemVar

        listType =
            AppN ModuleName.list Name.list [ itemType ]
    in
    IO.do (IO.indexedMap_list (constrainListEntry rtv pos entryType) items) <| \itemCons ->
    [ Constraint.And itemCons
    , Constraint.Equal region Constraint.CategoryList listType expected
    ]
        |> Constraint.And
        |> Constraint.exists [ itemVar ]
        |> IO.return


constrainListEntry : RigidTypeVars -> Pos -> Type -> Int -> Expression -> IO Constraint
constrainListEntry rtv pos tipe index expr =
    constrain rtv expr (Constraint.Expected_FromContext pos (Constraint.Context_ListEntry index) tipe)


constrainLambda : RigidTypeVars -> Pos -> List CA.Pattern -> Expression -> Expected CA.Type -> IO Constraint
constrainLambda rtv pos args body expected =
    IO.do (constrainArguments args) <| \argsAcc ->
    let
        (Pattern.Acc headers pvars revCons) =
            argsAcc.patternAcc
    in
    IO.do (constrain rtv body (Constraint.Expected_NoExpectation argsAcc.resultType)) <| \bodyCon ->
    [ Constraint.Let
        { rigidVars = []
        , flexVars = pvars
        , header = headers
        , headerCon = Constraint.And (List.reverse revCons)
        , bodyCon = bodyCon
        }
    , Constraint.Equal pos Constraint.Category_Lambda argsAcc.ty expected
    ]
        |> CAnd
        |> Constraint.exists argsAcc.vars
        |> IO.return


constrainArguments : List CA.Pattern -> IO ArgsAcc
constrainArguments args =
    constrainArgumentsRec args Pattern.emptyState


constrainArgumentsRec : List CA.Pattern -> Pattern.Acc -> IO ArgsAcc
constrainArgumentsRec args state =
    case args of
        [] ->
            IO.do mkFlexVar <| \resultVar ->
            let
                resultType =
                    VarN resultVar
            in
            IO.return
                { vars = [ resultVar ]
                , ty = resultType
                , result = resultType
                , state = state
                }

        pattern :: otherArgs ->
            IO.do mkFlexVar <| \argVar ->
            let
                argType =
                    VarN argVar
            in
            IO.do (Pattern.add pattern (Constraint.Expected_NoExpectation argType) state) <| \updatedState ->
            IO.do (constrainArgumentsRec otherArgs updatedState) <| \nextArgs ->
            IO.return
                { vars = argVar :: nextArgs.vars
                , ty = FunN argType nextArgs.ty
                , result = nextArgs.result
                , state = nextArgs.newState
                }
