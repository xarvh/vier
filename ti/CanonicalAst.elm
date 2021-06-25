module CanonicalAst exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Name =
    String


type alias ModuleName =
    String


type alias Pos =
    ()


type alias TyVarRef =
    Int


type At a
    = At Pos a



---


type Type
    = TypeConstant Name (List Type)
    | TypeVariable TyVarRef
    | TypeFunction Type Bool Type
    | TypeRecord (Maybe String) (Dict String Type)
    | TypeAlias String Type


type Annotation
    = Annotation (Set Name) Type


type alias Union =
    { vars : List Name
    , alts : List UnionConstructor

    -- CACHE numAlts for exhaustiveness checking
    , numAlts : Int
    }


type alias UnionConstructor =
    { name : Name
    , index : Int
    , args : List Type

    -- CACHE ??
    --, argsLength : Int
    }



----
--- Expression
--


type alias Expression =
    At ExpressionRaw


type ExpressionRaw
    = Literal Literal
    | Variable VariableArgs
    | Constructor ModuleName Name Annotation
    | Lambda (List Pattern) Expression
    | Call Expression (List Expression)
    | List (List Expression)
    | If (List ( Expression, Expression )) Expression
    | Try Expression (List ( Pattern, Expression ))
    | Let Name (Maybe Annotation) Expression
    | Record (Maybe VariableArgs) (Dict String Expression)


type alias VariableArgs =
    --, attrPath : List String
    { name : String
    , annotation : Maybe Annotation
    , maybeForeign : Maybe ModuleName
    }


type Literal
    = LiteralString



----
--- Pattern
--


type alias Pattern =
    At PatternRaw


type PatternRaw
    = PatternAnything
    | PatternVariable Name
    | PatternLiteral Literal
    | PatternRecord (Dict Name Pattern)
    | PatternConstructor PatternConstructorParams


type alias PatternConstructorParams =
    { home : ModuleName
    , typeName : Name
    , union : Union
    , name : Name
    , index : Int
    , args : List PatternCtorArg
    }


type alias PatternCtorArg =
    -- CACHE for destructors/errors
    { index : Int

    -- CACHE for type inference
    , ty : Type
    , arg : Pattern
    }
