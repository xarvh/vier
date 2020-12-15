module Types.FormattableAst exposing (..)

{-| This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

-}

{- TODO: if possible, replace all OneOrMore with List -}

import OneOrMore exposing (OneOrMore)


type alias Module =
    List Statement


type alias DefinitionArgs =
    { name : Pattern
    , maybeAnnotation : Maybe Type
    , parameters : List Pattern
    , body : OneOrMore Statement
    }


type Statement
    = Evaluation Expression
    | Definition DefinitionArgs
    | Mutation
        { left : String
        , mutop : String
        , right : Expression
        }
    | TypeAlias
        { name : String
        , args : List String
        , type_ : Type
        }
    | TypeDefinition
        { name : String
        , args : List String
        , constructors : List TypeConstructor
        }


type alias TypeConstructor =
    { name : String
    , args : List Type
    }


{-| Unlike the canonical annotation, the formattable annotation allows the mutability flag even where it's invalid.
This way we can tell the user why they can't flag those as mutable rather than just producing a syntax error.
-}
type Type
    = TypeConstantOrVariable
        { name : String
        }
    | TypeFunction
        -- TODO TypeFunction's List is guaranteed to have at least TWO items, but I'm not yet sure what's the best format for them
        (List Type)
    | TypePolymorphic
        -- TODO name should be a String
        { name : Type
        , args : OneOrMore Type
        }
    | TypeTuple (List Type)
    | TypeRecord (List ( String, Type ))
    | TypeMutable Type


type Expression
    = StringLiteral
        { start : Int
        , end : Int
        , string : String
        }
    | NumberLiteral
        { start : Int
        , end : Int
        , number : String
        }
    | Variable
        { start : Int
        , end : Int
        , variable : String
        , willBeMutated : Bool
        }
    | Lambda
        { start : Int

        -- TODO this should be a list
        , parameters : OneOrMore Pattern
        , body : OneOrMore Statement
        }
    | FunctionCall
        { reference : Expression
        , arguments : OneOrMore Expression
        }
    | Binop
        { left : Expression
        , op : String
        , right : Expression
        }
    | Unop
        { start : Int
        , op : String
        , right : Expression
        }
    | If
        { start : Int
        , isOneLine : Bool
        , condition : Expression
        , true : Expression
        , false : Expression
        }
    | Match
        { start : Int
        , isOneLine : Bool
        , value : Expression
        , patterns : List ( Pattern, Expression )
        , maybeElse : Maybe Expression
        }
    | Tuple (List Expression)
    | Record
        (List
            { name : String
            , value : Expression
            }
        )


type Pattern
    = PatternAny String


exprStart : Expression -> Int
exprStart expr =
    case expr of
        StringLiteral { start, end, string } ->
            start

        NumberLiteral { start, end, number } ->
            start

        Variable { start, end, variable } ->
            start

        Lambda { start, parameters, body } ->
            start

        FunctionCall { reference, arguments } ->
            exprStart reference

        Binop { left, op, right } ->
            exprStart left

        Unop { start, op, right } ->
            start

        If { start, condition, true, false } ->
            start

        Match { start, value, patterns, maybeElse } ->
            start

        Tuple _ ->
            Debug.todo ""

        Record attrs ->
            Debug.todo ""
