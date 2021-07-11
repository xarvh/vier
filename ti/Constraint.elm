module Constraint exposing (..)

import CanonicalAst as CA exposing (At, Name, Pos)
import Dict exposing (Dict)
import Type exposing (Type)



--
-- Expressions: Category/Expected/Context
--


{-| TODO rename this to ExpressionCategory?
-}
type Category
    = Category_Literal
    | Category_List
    | Category_Lambda
    | Category_Record
    | Category_Local Name
    | Category_Foreign Name


type Expected ty
    = Expected_NoExpectation ty
      --     | Expected_FromAnnotation Name Int SubContext ty
    | Expected_FromContext Pos Context ty


type
    Context
    --          | Negate
    --          | OpLeft Name.Name
    --          | OpRight Name.Name
    --          | IfCondition
    --          | IfBranch Index.ZeroBased
    --          | CaseBranch Index.ZeroBased
    --          | CallArity MaybeName Int
    --          | CallArg MaybeName Index.ZeroBased
    --          | RecordAccess A.Region (Maybe Name.Name) A.Region Name.Name
    --          | RecordUpdateKeys Name.Name (Map.Map Name.Name Can.FieldUpdate)
    --          | RecordUpdateValue Name.Name
    --          | Destructure
    = Context_ListEntry Int



--
-- Patterns: Category/Expected/Context
--


type PatternExpected ty
    = PatternExpected_NoExpectation ty
    | PatternExpected_FromContext Pos PatternContext ty


type PatternCategory
    = PatternCategory_Constructor Name


type PatternContext
    = PatternContext_ConstructorArg Name Int



-- type SubContext
--          = TypedIfBranch Index.ZeroBased
--          | TypedCaseBranch Index.ZeroBased
-- | TypedBody


type Constraint
    = True_
    | And (List Constraint)
    | Local Pos Name (Expected Type)
      -- this is for variable whose type is already known
    | Foreign Pos Name CA.Annotation (Expected Type)
    | Pattern Pos PatternCategory Type (PatternExpected Type)
    | Equal Pos Category Type (Expected Type)
    | Let
        { rigidVars : List Type.Variable
        , flexVars : List Type.Variable
        , header : Dict Name (At Type)
        , headerCon : Constraint
        , bodyCon : Constraint
        }


exists : List Type.Variable -> Constraint -> Constraint
exists flexVars constraint =
    Let
        { rigidVars = []
        , flexVars = flexVars
        , header = Dict.empty
        , headerCon = constraint
        , bodyCon = True_
        }


ptypeReplace : PatternExpected a -> b -> PatternExpected b
ptypeReplace expectation ty =
    case expectation of
        PatternExpected_NoExpectation _ ->
            PatternExpected_NoExpectation ty

        PatternExpected_FromContext pos context _ ->
            PatternExpected_FromContext pos context ty


typeReplace : Expected a -> b -> Expected b
typeReplace expectation ty =
    case expectation of
        Expected_NoExpectation _ ->
            Expected_NoExpectation ty

        --     Expected_FromAnnotation name arity context _ ->
        --       Expected_FromAnnotation name arity context tipe
        Expected_FromContext region context _ ->
            Expected_FromContext region context ty
