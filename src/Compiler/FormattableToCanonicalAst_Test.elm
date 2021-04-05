module Compiler.FormattableToCanonicalAst_Test exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.TestHelpers exposing (p)
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA
import Types.Literal as Literal


type alias Name =
    String


tests : Test
tests =
    Test.Group "FormattableToCanonicalAst"
        [ unionTypes
        , binops
        , tuples
        , lists
        , moduleAndAttributePaths
        , records
        , patterns
        , annotations
        , pipes
        , functions
        ]



----
---
--


simpleTest =
    Test.simple Debug.toString


codeTest =
    Test.codeTest Debug.toString


isOk =
    Test.isOk Debug.toString


hasError =
    Test.hasError Debug.toString


firstDefinition : String -> String -> Result String CA.ValueDef
firstDefinition name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "Dict fail")


firstEvaluation : String -> String -> Result String CA.Expression
firstEvaluation name code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "findValue fail")
        |> Result.andThen (\def -> List.head def.body |> Result.fromMaybe "head fail")
        |> Result.andThen (asEvaluation >> Result.fromMaybe "asEval fail")


stringToCanonicalModule code =
    code
        |> Compiler.TestHelpers.stringToCanonicalModule
        |> Compiler.TestHelpers.resErrorToString


asEvaluation : CA.Statement -> Maybe CA.Expression
asEvaluation s =
    case s of
        CA.Evaluation expr ->
            Just expr

        _ ->
            Nothing



----
--- Union Types
--


unionTypes : Test
unionTypes =
    Test.Group "unionTypes"
        [ hasError
            { name = "name starts with uppercase"
            , run = \_ -> stringToCanonicalModule "union a = A"
            , test = Test.errorShouldContain "uppercase"
            }
        , hasError
            { name = "constructor names start with uppercase"
            , run = \_ -> stringToCanonicalModule "union A = a"
            , test = Test.errorShouldContain "constructor"
            }
        , hasError
            { name = "tuples op precedence"
            , run = \_ -> stringToCanonicalModule "union A = X Bool & Bool"
            , test = Test.errorShouldContain "operators"
            }
        , isOk
            { name = "tuples op precedence works with parens"
            , run = \_ -> stringToCanonicalModule "union A = X (Bool & Bool)"
            }
        ]


binops : Test
binops =
    Test.Group "Binops"
        [ simpleTest
            { name = "left-association"
            , run = \_ -> firstEvaluation "a" "a = 1 + 2 + 3"
            , expected =
                Ok
                    (CA.Call p
                        (CA.Call p
                            (CA.Variable p { name = "+", isRoot = True, attrPath = [] })
                            (CA.ArgumentExpression (CA.Literal p (Literal.Number "3")))
                        )
                        (CA.ArgumentExpression
                            (CA.Call p
                                (CA.Call p
                                    (CA.Variable p { name = "+", isRoot = True, attrPath = [] })
                                    (CA.ArgumentExpression (CA.Literal p (Literal.Number "2")))
                                )
                                (CA.ArgumentExpression (CA.Literal p (Literal.Number "1")))
                            )
                        )
                    )
            }
        , simpleTest
            { name = "precedence"
            , run = \_ -> firstEvaluation "a" "a = 1 + 2 * 3"
            , expected =
                Ok
                    (CA.Call p
                        (CA.Call p
                            (CA.Variable p { name = "+", isRoot = True, attrPath = [] })
                            (CA.ArgumentExpression
                                (CA.Call p
                                    (CA.Call p
                                        (CA.Variable p { name = "*", isRoot = True, attrPath = [] })
                                        (CA.ArgumentExpression (CA.Literal p (Literal.Number "3")))
                                    )
                                    (CA.ArgumentExpression (CA.Literal p (Literal.Number "2")))
                                )
                            )
                        )
                        (CA.ArgumentExpression (CA.Literal p (Literal.Number "1")))
                    )
            }
        , codeTest "functional notation"
            """
            a = (-)
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Variable p { name = "-", isRoot = True, attrPath = [] }
            )
        ]



----
--- Lists
--


lists : Test
lists =
    Test.Group "Lists"
        [ simpleTest
            { name = "list type sugar"
            , run =
                \_ ->
                    firstDefinition "l"
                        """
                        l : [ SPCore.Bool ]
                        l = l
                        """
            , expected =
                Ok
                    { body = [ CA.Evaluation (CA.Variable p { name = "Test.l", isRoot = True, attrPath = [] }) ]
                    , maybeAnnotation =
                        Just
                            (CA.TypeConstant p
                                "SPCore.List"
                                [ CA.TypeConstant p "SPCore.Bool" [] ]
                            )
                    , mutable = False
                    , pattern = CA.PatternAny p "Test.l"
                    }
            }
        ]



----
--- Tuples
--


tuples : Test
tuples =
    Test.Group "Tuples"
        [ simpleTest
            { name = "tuple2"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2"
            , expected =
                Ok
                    (CA.Record p
                        Nothing
                        (Dict.fromList
                            [ ( "first", CA.Literal p (Literal.Number "1") )
                            , ( "second", CA.Literal p (Literal.Number "2") )
                            ]
                        )
                    )
            }
        , simpleTest
            { name = "tuple3"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2 & 3"
            , expected =
                Ok
                    (CA.Record p
                        Nothing
                        (Dict.fromList
                            [ ( "first", CA.Literal p (Literal.Number "1") )
                            , ( "second", CA.Literal p (Literal.Number "2") )
                            , ( "third", CA.Literal p (Literal.Number "3") )
                            ]
                        )
                    )
            }
        , hasError
            { name = "tuple4"
            , run = \_ -> firstEvaluation "a" "a = 1 & 2 & 3 & 4"
            , test = Test.errorShouldContain "use a record"
            }
        , simpleTest
            { name = "tuple2 type"
            , run =
                \_ ->
                    firstDefinition "a"
                        """
                        a : Blah & Blah
                        a = a
                        """
            , expected =
                Ok
                    { body = [ CA.Evaluation (CA.Variable p { name = "Test.a", attrPath = [], isRoot = True }) ]
                    , maybeAnnotation =
                        Just
                            (CA.TypeRecord p
                                Nothing
                                (Dict.fromList
                                    [ ( "first", CA.TypeConstant p "Test.Blah" [] )
                                    , ( "second", CA.TypeConstant p "Test.Blah" [] )
                                    ]
                                )
                            )
                    , mutable = False
                    , pattern = CA.PatternAny p "Test.a"
                    }
            }
        , hasError
            { name = "tuple4, type"
            , run =
                \_ ->
                    firstDefinition "a"
                        """
                        a : Blah & Blah & Blah & Blah
                        a = a
                        """
            , test = Test.errorShouldContain "Use a record"
            }
        ]



----
--- Module and Attribute Paths
--


moduleAndAttributePaths : Test
moduleAndAttributePaths =
    let
        accept s =
            isOk { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s) }

        reject s m =
            hasError { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s), test = Test.errorShouldContain m }
    in
    Test.Group "Module and Attribute Paths"
        [ accept "blah.blah.blah"
        , reject "Blah.Blah.blah" "Constructor"
        , reject "blah.Blah.blah" "lower"
        , reject "Blah.blah.Blah" "lower"
        , reject "Blah..blah" "dot"
        , reject ".Blah" "shorthand"
        , reject ".blah.blah" "shorthand"
        , reject ".blah" "shorthand"
        , reject "..." ""
        , accept "x .. y"
        ]



----
--- Records
--


records : Test
records =
    let
        accept s =
            isOk { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s) }

        reject s m =
            hasError { name = s, run = \_ -> firstDefinition "a" ("a = " ++ s), test = Test.errorShouldContain m }
    in
    Test.Group "Records"
        [ simpleTest
            { name = "functional update"
            , run = \_ -> firstEvaluation "a" "a = { m with b, c = 1 }"
            , expected =
                [ ( "c", CA.Literal p (Literal.Number "1") )
                , ( "b", CA.Variable p { attrPath = [], name = "Test.b", isRoot = True } )
                ]
                    |> Dict.fromList
                    |> CA.Record p (Just { isRoot = True, attrPath = [], name = "Test.m" })
                    |> Ok
            }
        , simpleTest
            { name = "update shorthand"
            , run = \_ -> firstEvaluation "b" "b = { a.k with y = .x }"
            , expected =
                Dict.singleton "y" (CA.Variable p { isRoot = True, attrPath = [ "k", "x" ], name = "Test.a" })
                    |> CA.Record p (Just { isRoot = True, attrPath = [ "k" ], name = "Test.a" })
                    |> Ok
            }
        , codeTest "annotation, extensible"
            """
            a : { b with x : Bool }
            a = a
            """
            (firstEvaluation "a")
            (Test.errContain "disabled")
        ]



----
--- Pattern
--


patterns : Test
patterns =
    Test.Group "Patterns"
        [ hasError
            { name = "can't declare functions inside patterns "
            , run =
                \_ ->
                    """
                    x =
                      c (a b) = 2
                    """
                        |> firstEvaluation "x"
            , test = Test.errorShouldContain "function"
            }
        , codeTest "[reg] record patterns are NOT extensible"
            """
            a =
              { b with c } = d
            """
            (firstEvaluation "a")
            (Test.errContain "with")
        ]



----
--- Annotations
--


annotations : Test
annotations =
    Test.Group "Annotations"
        [ hasError
            { name = "annotation mutability must match definition's"
            , run =
                \_ ->
                    """
                    a : Number
                    a @= 3
                    """
                        |> firstEvaluation "a"
            , test = Test.errorShouldContain "mutability"
            }
        , hasError
            { name = "annotation mutability must match definition's"
            , run =
                \_ ->
                    """
                    a : Number
                    b = 3
                    """
                        |> firstEvaluation "b"
            , test = Test.errorShouldContain "name"
            }
        ]



----
--- Pipes
--


pipes : Test
pipes =
    Test.Group "Pipes"
        [ codeTest "sendLeft is inlined"
            """
            a = thing >> function
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Call p
                    (CA.Variable p { name = "Test.function", isRoot = True, attrPath = [] })
                    (CA.ArgumentExpression <| CA.Variable p { name = "Test.thing", isRoot = True, attrPath = [] })
            )
        , codeTest "sendRight is inlined"
            """
            a = function << thing
            """
            (firstEvaluation "a")
            (Test.okEqual <|
                CA.Call p
                    (CA.Variable p { name = "Test.function", isRoot = True, attrPath = [] })
                    (CA.ArgumentExpression <| CA.Variable p { name = "Test.thing", isRoot = True, attrPath = [] })
            )
        ]



----
--- Functions
--


functions : Test
functions =
    Test.Group "Functions"
        [ codeTest "[rec] lambda with two arguments"
            """
            f =
              fn a b = 1
            """
            (firstEvaluation "f")
            Test.justOk
        , let
            findABC mod =
                Maybe.map3
                    (\a b c -> ( a, b, c ))
                    (CA.findValue "Test.a" mod)
                    (CA.findValue "Test.b" mod)
                    (CA.findValue "Test.c" mod)

            transform code =
                code
                    |> Compiler.TestHelpers.stringToCanonicalModule
                    |> Compiler.TestHelpers.resErrorToString
                    |> Result.andThen (findABC >> Result.fromMaybe "findABC fail")
          in
          codeTest "short function notation"
            """
            a x y z = x + y + z
            b = fn x y z = x + y + z
            c = fn x = fn y = fn z = x + y + z
            """
            transform
            (Test.freeform <| \( a, b, c ) ->
            if a.body == b.body && b.body == c.body then
                Nothing

            else
                [ "The three don't match:"
                , Debug.toString a
                , Debug.toString b
                , Debug.toString c
                ]
                    |> String.join "\n"
                    |> Just
            )
        ]
