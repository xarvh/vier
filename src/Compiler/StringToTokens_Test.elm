module Compiler.StringToTokens_Test exposing (..)

import Test exposing (Test)
import Types.Error exposing (Error)
import Compiler.StringToTokens as Lexer
import Types.Token as Token exposing (Token)


simpleTest =
    Test.simple Debug.toString


lexTokens : String -> () -> Result Error (List Token)
lexTokens s _ =
    Lexer.lexer s


tests : List Test
tests =
    [ ----
      --- Unary additive ops
      --
      simpleTest
        { name = "Unary +/-: -a"
        , run = lexTokens " -a"
        , expected =
            Ok
                [ { kind = Token.BlockStart, start = 0, end = 1 }
                , { kind = Token.Unop "-", start = 1, end = 2 }
                , { kind = Token.Symbol "a", start = 2, end = 3 }
                , { kind = Token.BlockEnd, start = 3, end = 3 }
                ]
        }
    , simpleTest
        { name = "Unary +/-: a - -a"
        , run = lexTokens "a - -a"
        , expected =
            Ok
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Symbol "a", start = 0, end = 1 }
                , { kind = Token.Binop Token.Addittive "-", start = 2, end = 3 }
                , { kind = Token.Unop "-", start = 4, end = 5 }
                , { kind = Token.Symbol "a", start = 5, end = 6 }
                ]
        }
    , simpleTest
        { name = "Unary +/-: a-a"
        , run = lexTokens "a-a"
        , expected =
            Ok
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Symbol "a", start = 0, end = 1 }
                , { kind = Token.Binop Token.Addittive "-", start = 1, end = 2 }
                , { kind = Token.Symbol "a", start = 2, end = 3 }
                ]
        }

    , simpleTest
        { name = "Arrow ->"
        , run = lexTokens "->"
        , expected =
            Ok
                [ { kind = Token.NewSiblingLine, start = 0, end = 0 }
                , { kind = Token.Arrow, start = 0, end = 2 }
                ]
        }

    ----
    --- Blocks, sibling lines, indentation
    --
    , simpleTest
        { name = ""
        , run = lexTokens """
a =
 1
b = 1
"""
        , expected =
            Ok
                [ { kind = Token.NewSiblingLine, start = 1, end = 1 }
                , { kind = Token.Symbol "a", start = 1, end = 2 }
                , { kind = Token.Defop, start = 3, end = 4 }
                , { kind = Token.BlockStart, start = 5, end = 6 }
                , { kind = Token.NumberLiteral "1", start = 6, end = 7 }
                , { kind = Token.BlockEnd, start = 8, end = 8 }
                , { kind = Token.NewSiblingLine, start = 8, end = 8 }
                , { kind = Token.Symbol "b", start = 8, end = 9 }
                , { kind = Token.Defop, start = 10, end = 11 }
                , { kind = Token.NumberLiteral "1", start = 12, end = 13 }
                ]
        }
    ]
