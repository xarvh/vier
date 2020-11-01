module Vier.Syntax exposing (..)

import Parser exposing (consumeOne, do, fail, oneOf, optional, succeed)
import Vier.Lexer.Indent as Indent
import Vier.Lexer.Token as Token exposing (OpenOrClosed(..), TokenKind(..))


type alias IndentedKind =
    {- Indent.Indented -}
    TokenKind


indentedTokenToIndentedKind it =
    case it of
        Indent.Structure structure ->
            --Indent.Structure structure
            Nothing

        Indent.Content token ->
            Just token.kind


d name =
    --     Parser.doWithDebug (\{ path, first } -> Debug.log "d" ( path, first )) name
    do


w name parser =
    --     d name parser succeed
    parser


su : String -> a -> Parser a
su name a =
    d name (succeed a) succeed


type alias Parser a =
    Parser.Parser TokenKind (List TokenKind) a


type Expression
    = Literal String
    | Variable String
    | FunctionCall Expression ( Expression, List Expression )
    | Binop Expression String Expression
    | Unop String Expression
    | If { condition : Expression, true : Expression, false : Expression }
    | Error


parse : List Token.IndentedToken -> Maybe Expression
parse its =
    let
        uncons : List a -> Maybe ( a, List a )
        uncons ls =
            case ls of
                head :: tail ->
                    Just ( head, tail )

                [] ->
                    Nothing

        parser =
            d "root expr" expr <| \a ->
            d "root end" Parser.end <| \b ->
            succeed a
    in
    its
        |> List.filterMap indentedTokenToIndentedKind
        |> parser uncons []
        |> Maybe.map Tuple.first


tokenKind : Parser TokenKind
tokenKind =
    consumeOne



{-
   do consumeOne <| \indentedToken ->
   case indentedToken of
       Indent.Structure structure ->
           fail

       Indent.Content kind ->
           su kind
-}
----
--- Term
--


term : Parser Expression
term =
    d "tokenKind" tokenKind <| \kind ->
    case kind of
        Token.NumberLiteral s ->
            su "nl" <| Literal s

        Token.StringLiteral s ->
            su "sl" <| Literal s

        Token.Symbol s ->
            su s <| Variable s

        _ ->
            fail



{- Precedence rules:

   ()

   f a b ------------> function application

   not, risk --------> unary

   ^ ----------------> exp

   * / --------------> multiplicative

   + - ++ -----------> addittive

   >= <= == =/= -----> comparison

   and, or, xor -----> logical

   |> <| >> << ------> pipes

   := += -= /= *= ---> assignments

-}


expr : Parser Expression
expr =
    Parser.expression term
        [ parens
        , functionApplication
        , unops
        , binops [ "^" ]
        , binops [ "*", "/" ]
        , binops [ "+", "-", "++" ]
        , binops [ ">=", "<=", "==", "=/=" ]

        -- TODO pipes can't actually be mixed
        , binops [ "|>", "<|", "<<", ">>" ]
        , binops [ ":=", "+=", "-=", "/=", "*=" ]
        ]



----
--- Parens
--


parens : Parser Expression -> Parser Expression
parens higher =
    oneOf
        [ higher
        , surroundWith (RoundParen Open) (RoundParen Closed) (Parser.breakCircularDefinition <| \_ -> expr)
        ]


surroundWith : TokenKind -> TokenKind -> Parser a -> Parser a
surroundWith left right =
    Parser.surroundWith (exactTokenKind left) (exactTokenKind right)


exactTokenKind : TokenKind -> Parser ()
exactTokenKind targetKind =
    do tokenKind <| \kind ->
    if targetKind == kind then
        succeed ()

    else
        fail



----
--- Function application
--


functionApplication : Parser Expression -> Parser Expression
functionApplication higher =
    do higher <| \e ->
    do (Parser.zeroOrMore higher) <| \es ->
    case es of
        argsHead :: argsTail ->
            succeed <| FunctionCall e ( argsHead, argsTail )

        [] ->
            succeed e



----
--- Unops
--


unops : Parser Expression -> Parser Expression
unops higher =
    do (optional unaryOperator) <| \maybeUnary ->
    do higher <| \right ->
    case maybeUnary of
        Just op ->
            su "unop" <| Unop op right

        Nothing ->
            succeed right


unaryOperator : Parser String
unaryOperator =
    do tokenKind <| \kind ->
    case kind of
        Token.Unop s ->
            succeed s

        _ ->
            fail



----
--- Binops
--


binops : List String -> Parser Expression -> Parser Expression
binops ops higher =
    let
        binopAndPrev : Parser ( String, Expression )
        binopAndPrev =
            Parser.tuple2 (binaryOperators ops) higher
    in
    do higher <| \left ->
    do (optional binopAndPrev) <| \maybeBinopAndPrev ->
    case maybeBinopAndPrev of
        Nothing ->
            succeed left

        Just ( op, right ) ->
            succeed <| Binop left op right


binaryOperators : List String -> Parser String
binaryOperators ops =
    do tokenKind <| \kind ->
    case kind of
        Token.Binop s ->
            -- TODO would a Set be faster? How do we ensure that he conversion to Set is not ran every time?
            -- It's probably better if the tokenizer sets the binop "precedence group" already
            if List.member s ops then
                succeed s

            else
                fail

        _ ->
            fail
