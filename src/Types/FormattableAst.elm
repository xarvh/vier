module Types.FormattableAst exposing (..)

{-| This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

-}

import SepList exposing (SepList)
import Types.Literal
import Types.Token


type alias Pos =
    ( Int, Int )


type alias Module =
    List Statement


type alias ValueDef =
    { pattern : Pattern
    , mutable : Bool
    , maybeAnnotation : Maybe Annotation
    , body : List Statement

    -- TODO move it in the statement constructor?
    , pos : Pos
    }


type alias Annotation =
    { name : String
    , mutable : Bool
    , ty : Type
    }


type Statement
    = Evaluation Expression
    | Definition ValueDef
    | TypeAlias
        { name : String
        , args : List String
        , ty : Type
        }
    | UnionDef
        { name : String
        , args : List String

        -- constructors are parsed into a TypePolymorphic
        , constructors : List Type
        }


type Type
    = TypeName Pos String
    | TypePolymorphic Pos String (List Type)
    | TypeFunction Pos Type Bool Type
    | TypeTuple Pos (List Type)
    | TypeRecord Pos (RecordArgs Type)


type Expression
    = Literal Pos Types.Literal.Value
    | Variable Pos { isBinop : Bool } String
    | Mutable Pos String
    | Lambda Pos (List Pattern) (List Statement)
    | FunctionCall Pos Expression (List Expression)
    | Binop Pos Types.Token.PrecedenceGroup (SepList String Expression)
    | Unop Pos String Expression
    | If
        Pos
        { isOneLine : Bool
        , condition : Expression
        , true : List Statement
        , false : List Statement
        }
    | Try
        Pos
        { isOneLine : Bool
        , value : Expression
        , patterns : List ( Pattern, List Statement )
        , maybeElse : Maybe (List Statement)
        }
    | Record Pos (RecordArgs Expression)
    | List Pos (List Expression)


type Pattern
    = PatternAny Pos String
    | PatternLiteral Pos Types.Literal.Value
    | PatternApplication Pos String (List Pattern)
    | PatternList Pos (List Pattern)
    | PatternRecord Pos (RecordArgs Pattern)
    | PatternCons Pos Pattern Pattern
    | PatternTuple Pos (List Pattern)


type alias RecordArgs expr =
    { extends : Maybe expr
    , attrs : List ( String, Maybe expr )
    }


recordArgs_map : (a -> b) -> RecordArgs a -> RecordArgs b
recordArgs_map f ar =
    { extends = Maybe.map f ar.extends
    , attrs = List.map (Tuple.mapSecond (Maybe.map f)) ar.attrs
    }



----
--- Helpers
--


patternPos : Pattern -> Pos
patternPos pa =
    case pa of
        PatternAny p _ ->
            p

        PatternLiteral p _ ->
            p

        PatternApplication p _ _ ->
            p

        PatternList p _ ->
            p

        PatternRecord p _ ->
            p

        PatternCons p _ _ ->
            p

        PatternTuple p _ ->
            p


posMap_statement : (Pos -> Pos) -> Statement -> Statement
posMap_statement f stat =
    case stat of
        Evaluation e ->
            Evaluation (posMap_expression f e)

        Definition def ->
            Definition
                { pattern = posMap_pattern f def.pattern
                , mutable = def.mutable
                , maybeAnnotation = Maybe.map (\ann -> { ann | ty = posMap_type f ann.ty }) def.maybeAnnotation
                , body = List.map (posMap_statement f) def.body
                , pos = f def.pos
                }

        TypeAlias ar ->
            TypeAlias
                { name = ar.name
                , args = ar.args
                , ty = posMap_type f ar.ty
                }

        UnionDef ar ->
            UnionDef
                { name = ar.name
                , args = ar.args
                , constructors = List.map (posMap_type f) ar.constructors
                }


posMap_expression : (Pos -> Pos) -> Expression -> Expression
posMap_expression f expr =
    case expr of
        Literal pos value ->
            Literal (f pos) value

        Variable pos isbin name ->
            Variable (f pos) isbin name

        Mutable pos name ->
            Mutable (f pos) name

        Lambda pos pas stats ->
            Lambda (f pos)
                (List.map (posMap_pattern f) pas)
                (List.map (posMap_statement f) stats)

        FunctionCall pos ref args ->
            FunctionCall (f pos)
                (posMap_expression f ref)
                (List.map (posMap_expression f) args)

        Binop pos group sepList ->
            Binop (f pos) group (SepList.mapItem (posMap_expression f) sepList)

        Unop pos name right ->
            Unop (f pos) name (posMap_expression f right)

        If pos ar ->
            If (f pos)
                { isOneLine = ar.isOneLine
                , condition = posMap_expression f ar.condition
                , true = List.map (posMap_statement f) ar.true
                , false = List.map (posMap_statement f) ar.false
                }

        Try pos ar ->
            Try (f pos)
                { isOneLine = ar.isOneLine
                , value = posMap_expression f ar.value
                , patterns = List.map (Tuple.mapBoth (posMap_pattern f) (List.map (posMap_statement f))) ar.patterns
                , maybeElse = Maybe.map (List.map (posMap_statement f)) ar.maybeElse
                }

        Record pos ar ->
            Record (f pos) (recordArgs_map (posMap_expression f) ar)

        List pos exs ->
            List (f pos) (List.map (posMap_expression f) exs)


posMap_pattern : (Pos -> Pos) -> Pattern -> Pattern
posMap_pattern f pa =
    case pa of
        PatternAny pos name ->
            PatternAny (f pos) name

        PatternLiteral pos val ->
            PatternLiteral (f pos) val

        PatternApplication pos cons pas ->
            PatternApplication (f pos) cons (List.map (posMap_pattern f) pas)

        PatternList pos pas ->
            PatternList (f pos) (List.map (posMap_pattern f) pas)

        PatternRecord pos ar ->
            PatternRecord (f pos) (recordArgs_map (posMap_pattern f) ar)

        PatternCons pos left right ->
            PatternCons (f pos)
                (posMap_pattern f left)
                (posMap_pattern f right)

        PatternTuple pos pas ->
            PatternTuple (f pos) (List.map (posMap_pattern f) pas)


posMap_type : (Pos -> Pos) -> Type -> Type
posMap_type f ty =
    case ty of
        TypeName pos name ->
            TypeName (f pos) name

        TypePolymorphic pos name args ->
            TypePolymorphic (f pos) name (List.map (posMap_type f) args)

        TypeFunction pos from mut to ->
            TypeFunction (f pos) (posMap_type f from) mut (posMap_type f to)

        TypeTuple pos tys ->
            TypeTuple (f pos) (List.map (posMap_type f) tys)

        TypeRecord pos ar ->
            TypeRecord (f pos) (recordArgs_map (posMap_type f) ar)
