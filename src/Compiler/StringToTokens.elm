module Compiler.StringToTokens exposing (..)

{-| I wanted to use patterns like `char0 :: char1 :: rest` to run most of the lexing,
on the assumption that Elm can execute that fast and because it was nice to read.

This choice forced me to keep track of both the `List Char` that represents the code,
and of the current position in the read buffer: the two are kept in sync manually and
awkwardly.

Eventually, I hope to rewrite the whole compiler in a different language that allows
better random access to strings (which also makes the premature optimisation even more
pointless, but hey, I'm having fun).

-}

import Regex exposing (Regex)
import Types.Error as Error exposing (Error, Res)
import Types.Token as Token exposing (Token)


type alias ReadState =
    { pos : Int
    , codeAsString : String
    , code : List Char
    , multiCommentDepth : Int
    , indentStack : List Int
    , maybeIndentToAdd : Maybe Int
    , accum : List Token
    }



----
--- Main
--


lexer : String -> Res (List Token)
lexer codeAsString =
    { pos = -1
    , multiCommentDepth = 0
    , code = '\n' :: String.toList codeAsString
    , codeAsString = codeAsString
    , indentStack = []
    , maybeIndentToAdd = Just 0
    , accum = []
    }
        |> lexerStep


lexerStep : ReadState -> Res (List Token)
lexerStep prevState =
    case lexContent prevState.pos prevState of
        Err a ->
            Err a

        Ok nextState ->
            -- TODO assert that each iteration eats at least one char
            if nextState.code /= [] then
                lexerStep nextState

            else
                nextState
                    |> stateToFinalTokens
                    |> List.reverse
                    |> Ok


stateToFinalTokens : ReadState -> List Token
stateToFinalTokens state =
    let
        blockEnd : Token
        blockEnd =
            { kind = Token.BlockEnd
            , start = state.pos
            , end = state.pos
            }
    in
    -- add remaining BlockEnds
    List.foldl (\stack accum -> blockEnd :: accum) state.accum state.indentStack


lexContent : Int -> ReadState -> Res ReadState
lexContent startPos state =
    let
        runLexer contentAhead lex posOffset rest =
            Ok state
                |> chainIf (contentAhead || state.pos > startPos) (addIndentTokens startPos)
                |> chainIf (state.pos > startPos) (contentLineToTokens startPos)
                |> Result.andThen (\s -> lex { s | code = rest, pos = s.pos + posOffset })
    in
    -- TODO single char lexing
    case state.code of
        '"' :: '"' :: '"' :: rest ->
            runLexer True (lexHardQuotedString state.pos) 3 rest

        '"' :: rest ->
            runLexer True (lexSoftQuotedString state.pos) 1 rest

        '#' :: rest ->
            runLexer False (lexSingleLineComment state.pos) 1 rest

        '[' :: '#' :: rest ->
            runLexer False (lexMultiLineComment state.pos) 2 rest

        '\n' :: rest ->
            runLexer False (lexIndent >> Ok) 1 rest

        '\t' :: rest ->
            Error.error state.pos Error.Tab

        char :: rest ->
            { state | pos = 1 + state.pos, code = rest }
                |> lexContent startPos

        [] ->
            runLexer False Ok 0 []


chainIf : Bool -> (state -> Result err state) -> Result err state -> Result err state
chainIf predicate fn result =
    result
        |> Result.andThen
            (\state ->
                if predicate then
                    fn state

                else
                    Ok state
            )


contentLineToTokens : Int -> ReadState -> Res ReadState
contentLineToTokens startPos state =
    let
        contentLine =
            String.slice startPos state.pos state.codeAsString
    in
    state.accum
        -- TODO (horrible) I'm adding a space in front so that indent will not
        -- eat all of the spaces in front of "  -a", so that `-` can be reocgnised as Unop
        |> contentLineToTokensRec (" " ++ contentLine) (startPos - 1)
        |> Result.map (\tokens -> { state | accum = tokens })


contentLineToTokensRec : String -> Int -> List Token -> Res (List Token)
contentLineToTokensRec untrimmedBlock untrimmedPos tokenAccu =
    case String.trimLeft untrimmedBlock of
        "" ->
            Ok tokenAccu

        codeBlock ->
            let
                spaces =
                    String.length untrimmedBlock - String.length codeBlock

                start =
                    untrimmedPos + spaces

                tryMatch ( regex, constructor ) =
                    case Regex.find regex untrimmedBlock of
                        match :: tail ->
                            Just ( match, constructor )

                        [] ->
                            Nothing
            in
            case mapFind tryMatch recognisedTokens of
                Nothing ->
                    codeBlock
                        |> String.split " "
                        |> List.take 1
                        |> String.join ""
                        |> Error.InvalidToken
                        |> Error.error start

                Just ( match, constructor ) ->
                    case constructor match.match of
                        Err kind ->
                            Error.error start kind

                        Ok ( tokenKind, charsConsumed ) ->
                            let
                                tokenStart =
                                    -- TODO maybe should just assert that match.index is 0?
                                    start + match.index

                                tokenEnd =
                                    tokenStart + charsConsumed - spaces

                                token =
                                    { kind = tokenKind
                                    , start = tokenStart
                                    , end = tokenEnd
                                    }

                                newBlock =
                                    String.dropLeft charsConsumed untrimmedBlock

                                accu =
                                    token :: tokenAccu
                            in
                            contentLineToTokensRec newBlock tokenEnd accu


recognisedTokens : List ( Regex, String -> Result Error.Kind ( Token.Kind, Int ) )
recognisedTokens =
    let
        reOrDie reString =
            case Regex.fromString reString of
                Nothing ->
                    Debug.todo "invalid regex!?"

                Just re ->
                    re

        recordEntryToTuple record =
            ( reOrDie record.regex
            , \match ->
                match
                    |> record.constructor
                    |> Result.map (\kind -> ( kind, record.consumed match ))
            )

        parenRegex s kind =
            { regex = "^[ ]*\\" ++ s
            , consumed = String.length
            , constructor = \match -> Ok kind
            }
    in
    List.map recordEntryToTuple
        [ -- Numbers
          { regex = "^[ ]*[0-9_]+[.]?[0-9_]*"
          , consumed = String.length
          , constructor = String.trimLeft >> Token.NumberLiteral >> Ok
          }
        , -- Words
          { regex = "^[ ]*[@]?[a-zA-Z._][a-zA-Z._0-9]*"
          , consumed = String.length
          , constructor =
                \m ->
                    let
                        match =
                            String.trimLeft m
                    in
                    Ok <|
                        case match of
--                             ".." ->
--                               Token.Binop Token.Addittive match

                            "fn" ->
                                Token.Fn

                            "if" ->
                                Token.If

                            "try" ->
                                Token.Try

                            "as" ->
                                Token.As

                            "then" ->
                                Token.Then

                            "else" ->
                                Token.Else

                            "with" ->
                                Token.With

                            "and" ->
                                Token.Binop Token.Logical match

                            "or" ->
                                Token.Binop Token.Logical match

                            "not" ->
                                Token.Unop match

                            _ ->
                                case String.uncons match of
                                    Nothing ->
                                        Debug.todo "not happening"

                                    Just ( head, tail ) ->
                                        if head == '@' then
                                            Token.Name { mutable = True } tail

                                        else
                                            Token.Name { mutable = False } match
          }

        -- Parens
        , parenRegex "(" <| Token.RoundParen Token.Open
        , parenRegex ")" <| Token.RoundParen Token.Closed
        , parenRegex "[" <| Token.SquareBracket Token.Open
        , parenRegex "]" <| Token.SquareBracket Token.Closed
        , parenRegex "{" <| Token.CurlyBrace Token.Open
        , parenRegex "}" <| Token.CurlyBrace Token.Closed
        , parenRegex "," <| Token.Comma
        , -- Unary addittive
          -- the `>` at the end is to avoid matching `->`
          { regex = "^[ ]+[+-][^ >=]"
          , consumed = \match -> String.length match - 1
          , constructor = String.trimLeft >> String.dropRight 1 >> Token.Unop >> Ok
          }
        , -- Squiggles
          { regex = "^[ ]*[=+\\-*/:><!&^|@]+"
          , consumed = String.length
          , constructor =
                \m ->
                    let
                        match =
                            String.trimLeft m
                    in
                    case match of
                        "->" ->
                            Ok <| Token.Arrow { mutable = False }

                        "@>" ->
                            Ok <| Token.Arrow { mutable = True }

                        ":" ->
                            Ok <| Token.HasType { mutable = False }

                        "@:" ->
                            Ok <| Token.HasType { mutable = True }

                        "^" ->
                            Ok <| Token.Binop Token.Exponential match

                        "*" ->
                            Ok <| Token.Binop Token.Multiplicative match

                        "/" ->
                            Ok <| Token.Binop Token.Multiplicative match

                        "+" ->
                            Ok <| Token.Binop Token.Addittive match

                        "-" ->
                            Ok <| Token.Binop Token.Addittive match

                        ">" ->
                            Ok <| Token.Binop Token.Comparison match

                        "<" ->
                            Ok <| Token.Binop Token.Comparison match

                        ">=" ->
                            Ok <| Token.Binop Token.Comparison match

                        "<=" ->
                            Ok <| Token.Binop Token.Comparison match

                        "==" ->
                            Ok <| Token.Binop Token.Comparison match

                        "=/=" ->
                            Ok <| Token.Binop Token.Comparison match

                        "&" ->
                            Ok <| Token.Binop Token.Tuple match

                        ":>" ->
                            Ok <| Token.Binop Token.Pipe match

                        "<:" ->
                            Ok <| Token.Binop Token.Pipe match

                        ">>" ->
                            Ok <| Token.Binop Token.Pipe match

                        "<<" ->
                            Ok <| Token.Binop Token.Pipe match

                        "=" ->
                            Ok <| Token.Defop { mutable = False }

                        "@=" ->
                            Ok <| Token.Defop { mutable = True }

                        ":=" ->
                            Ok <| Token.Binop Token.Mutop match

                        "+=" ->
                            Ok <| Token.Binop Token.Mutop match

                        "-=" ->
                            Ok <| Token.Binop Token.Mutop match

                        "/=" ->
                            -- TODO do we really want this op?
                            Ok <| Token.Binop Token.Mutop match

                        "*=" ->
                            Ok <| Token.Binop Token.Mutop match

                        "^=" ->
                            Ok <| Token.Binop Token.Mutop match

                        _ ->
                            Err <| Error.UnknownOperator match
          }
        ]



----
--- Block lexers
--


{-| TESTS

  - state.pos is updated manually, so it should be tested!

-}
lexSingleLineComment : Int -> ReadState -> Res ReadState
lexSingleLineComment startPos state =
    let
        ( length, rest ) =
            readWhile (\c -> c /= '\n') state.code

        endPos =
            state.pos + length
    in
    Ok
        { state
            | pos = endPos
            , code = rest
            , accum =
                { kind = Token.Comment
                , start = startPos
                , end = endPos
                }
                    :: state.accum
        }


{-| TESTS

  - `"\""` passes

  - `"\n"` passes

  - `"
    "` fails

  - `"sdf\\asdf\\"` passes

  - `"sdf\\\asdf\\"` passes

  - `"sdf\\asdf\\\"` fails

  - state.pos is updated manually, so it should be tested!

-}
lexSoftQuotedString : Int -> ReadState -> Res ReadState
lexSoftQuotedString startPos state =
    let
        rec pos isEscape code =
            case code of
                '\\' :: rest ->
                    rec (pos + 1) (not isEscape) rest

                '"' :: rest ->
                    let
                        endPos =
                            pos + 1
                    in
                    if isEscape then
                        rec endPos False rest

                    else
                        Ok
                            { state
                                | pos = endPos
                                , code = rest
                                , accum =
                                    { kind =
                                        state.codeAsString
                                            |> String.slice startPos endPos
                                            -- TODO transform escapes and reject non-escapable chars
                                            |> Token.StringLiteral
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                '\n' :: rest ->
                    -- https://www.reddit.com/r/ProgrammingLanguages/comments/l0ptdl/why_do_so_many_languages_not_allow_string/gjvrcg2/
                    Error.error pos Error.NewLineInsideSoftQuote

                char :: rest ->
                    rec (pos + 1) False rest

                [] ->
                    Error.error pos Error.UnterminatedStringLiteral
    in
    rec state.pos False state.code


{-| TESTS

  - """ passes

  - "\\n" passes

  - "
    " fails

  - "sdf\\asdf\\" passes

  - "sdf\\\\asdf\\" passes

  - "sdf\\asdf\\" fails

  - state.pos is updated manually, so it should be tested!

-}
lexHardQuotedString : Int -> ReadState -> Res ReadState
lexHardQuotedString startPos state =
    let
        rec pos isEscape doubleQuotes code =
            case code of
                '\\' :: rest ->
                    rec (pos + 1) (not isEscape) 0 rest

                '"' :: rest ->
                    let
                        endPos =
                            pos + 1
                    in
                    if isEscape then
                        rec endPos False 0 rest

                    else if doubleQuotes < 2 then
                        rec endPos False (doubleQuotes + 1) rest

                    else
                        Ok
                            { state
                                | pos = endPos
                                , code = rest
                                , accum =
                                    { kind =
                                        state.codeAsString
                                            |> String.slice startPos endPos
                                            -- TODO transform escapes and reject non-escapable chars
                                            |> Token.StringLiteral
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                char :: rest ->
                    rec (pos + 1) False 0 rest

                [] ->
                    Error.error pos Error.UnterminatedStringLiteral
    in
    rec state.pos False 0 state.code


{-| TESTS

  - properly nested comments should pass
  - improperly nested comments should fail
  - non-terminated comments should fail
  - state.pos is updated manually, so it should be tested!

-}
lexMultiLineComment : Int -> ReadState -> Res ReadState
lexMultiLineComment startPos state =
    let
        rec pos depth code =
            case code of
                '[' :: '#' :: rest ->
                    rec (pos + 1) (depth + 1) rest

                '#' :: ']' :: rest ->
                    let
                        endPos =
                            pos + 2
                    in
                    if depth > 0 then
                        rec endPos (depth - 1) rest

                    else
                        Ok
                            { state
                                | pos = endPos
                                , code = rest
                                , accum =
                                    { kind = Token.Comment
                                    , start = startPos
                                    , end = endPos
                                    }
                                        :: state.accum
                            }

                char :: rest ->
                    rec (pos + 1) depth rest

                [] ->
                    Error.error pos Error.UnterminatedMultiLineComment
    in
    rec state.pos 0 state.code


{-| LexIndent will successfully match anything, because a length 0 indent is valid.

NewSiblingLine, BlockStart, Block will be added only when we see that the line actually contains something.

-}
lexIndent : ReadState -> ReadState
lexIndent state =
    let
        ( newIndent, newCode ) =
            readWhile (\c -> c == ' ') state.code
    in
    { state
        | pos = newIndent + state.pos
        , code = newCode
        , maybeIndentToAdd = Just newIndent
    }


addIndentTokens : Int -> ReadState -> Res ReadState
addIndentTokens endPos state =
    case state.maybeIndentToAdd of
        Nothing ->
            Ok state

        Just newIndent ->
            addIndentTokensRec endPos newIndent True { state | maybeIndentToAdd = Nothing } state.indentStack


addIndentTokensRec : Int -> Int -> Bool -> ReadState -> List Int -> Res ReadState
addIndentTokensRec endPos newIndent isFirstRecursion state stack =
    let
        ( lastIndent, poppedStack ) =
            case stack of
                [] ->
                    ( 0, [] )

                head :: tail ->
                    ( head, tail )

        makeToken kind =
            { kind = kind
            , start = endPos - newIndent
            , end = endPos
            }
    in
    if newIndent == lastIndent then
        {-
           ```
           lastIndent
           newIndent
           ```

           ```
           previousRecursionIndent
             lastIndent
           newIndent
           ```
        -}
        Ok { state | accum = makeToken Token.NewSiblingLine :: state.accum, indentStack = stack }

    else if newIndent > lastIndent then
        if isFirstRecursion then
            {-
               ```
               lastIndent
                 newIndent
               ```
            -}
            Ok
                { state
                    | accum = makeToken Token.BlockStart :: state.accum
                    , indentStack = newIndent :: state.indentStack
                }

        else
            {- This is an error:
               ```
               previousRecursionIndent
                   lastIndent
                 newIndent
               ```
            -}
            { length = newIndent
            , stack = state.indentStack
            }
                |> Error.BadIndent
                |> Error.error endPos

    else
        addIndentTokensRec endPos newIndent False { state | accum = makeToken Token.BlockEnd :: state.accum } poppedStack


readWhile : (char -> Bool) -> List char -> ( Int, List char )
readWhile test =
    let
        rec : Int -> List char -> ( Int, List char )
        rec counter list =
            case list of
                [] ->
                    ( counter, list )

                head :: tail ->
                    if test head then
                        rec (counter + 1) tail

                    else
                        ( counter, list )
    in
    rec 0


{-| This is mostly so that I don't have to run a successful `f` twice
-}
mapFind : (a -> Maybe b) -> List a -> Maybe b
mapFind f ls =
    case ls of
        [] ->
            Nothing

        head :: tail ->
            case f head of
                Just b ->
                    Just b

                Nothing ->
                    mapFind f tail
