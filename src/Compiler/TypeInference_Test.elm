module Compiler.TypeInference_Test exposing (..)

import Compiler.CoreModule
import Compiler.TestHelpers as TH exposing (p)
import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Lib
import Prelude
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA exposing (Type)
import Types.Error exposing (Res)


tests : Test
tests =
    Test.Group "TypeInference"
        [ functions
        , statements
        , variableTypes
        , mutability
        , higherOrderTypes
        , records
        , patterns
        , try_as
        , if_then
        ]



----
---
--


codeTest =
    Test.codeTest Debug.toString


simpleTest =
    Test.simple Debug.toString


isOk =
    Test.isOk Debug.toString


hasError =
    Test.hasError Debug.toString


constant n =
    CA.TypeConstant p n []


function from to =
    CA.TypeFunction p from False to


tyNumber =
    constant "SPCore.Number"


tyNone =
    constant "SPCore.None"



----
--- These should be removed once we get rid of the old record declarations
--


typeFunction { from, fromIsMutable, to } =
    CA.TypeFunction p from fromIsMutable to


typeVariable { name } =
    CA.TypeVariable p name


typeConstant { ref, args } =
    CA.TypeConstant p ref args


typeRecord { extensible, attrs } =
    CA.TypeRecord p extensible attrs



----
---
--


infer : String -> String -> Result String TI.EnvEntry
infer name code =
    code
        |> TH.stringToCanonicalModuleWithPos
        |> Result.andThen (TI.inspectModule preamble)
        |> TH.resErrorToString
        |> Result.andThen
            (\( mod, env, subs ) ->
                env
                    |> Dict.get ("Test." ++ name)
                    |> Maybe.map normalizeSchema
                    |> Result.fromMaybe "Dict fail"
                    |> Result.map (\schema -> { schema | type_ = TH.removePos CA.extensionFold_type schema.type_ })
            )


preamble : TI.Env
preamble =
    let
        em x =
            { type_ = x
            , forall = Set.empty
            , mutable = False
            }
    in
    [ ( "Test.add", em <| function tyNumber (function tyNumber tyNumber) )
    , ( "Test.reset", em <| typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone } )
    ]
        |> Dict.fromList



----
--- "t2" -> "a"
--


normalizeSchema : TI.EnvEntry -> TI.EnvEntry
normalizeSchema schema =
    let
        ( ty, dict ) =
            normalizeType Dict.empty schema.type_

        replaceName name =
            Dict.get name dict |> Maybe.withDefault name
    in
    { schema
        | type_ = ty
        , forall = Set.map replaceName schema.forall
    }


normalizeName : Dict String String -> String -> ( String, Dict String String )
normalizeName dict name =
    case Dict.get name dict of
        Just new ->
            ( new, dict )

        Nothing ->
            if String.toInt name == Nothing then
                ( name, dict )

            else
                let
                    n =
                        Dict.size dict + 1 |> String.fromInt
                in
                ( n, Dict.insert name n dict )


normalizeType : Dict String String -> Type -> ( Type, Dict String String )
normalizeType dict ty =
    case ty of
        CA.TypeConstant pos name args ->
            let
                fold arg ( ars, d ) =
                    normalizeType d arg
                        |> Tuple.mapFirst (\na -> na :: ars)

                ( reversedArgs, dict1 ) =
                    List.foldl fold ( [], dict ) args
            in
            ( CA.TypeConstant pos name (List.reverse reversedArgs)
            , dict1
            )

        CA.TypeVariable pos name ->
            normalizeName dict name
                |> Tuple.mapFirst (CA.TypeVariable pos)

        CA.TypeFunction pos from0 fromIsMut to0 ->
            let
                ( from, d1 ) =
                    normalizeType dict from0

                ( to, d2 ) =
                    normalizeType d1 to0
            in
            ( CA.TypeFunction pos from fromIsMut to
            , d2
            )

        CA.TypeRecord pos ext0 attrs0 ->
            let
                ( et, d1 ) =
                    case ext0 of
                        Nothing ->
                            ( Nothing, dict )

                        Just e ->
                            normalizeName dict e |> Tuple.mapFirst Just

                fold name attr ( accum, d ) =
                    normalizeType d attr
                        |> Tuple.mapFirst (\na -> Dict.insert name na accum)

                ( attrs, d2 ) =
                    Dict.foldl fold ( Dict.empty, d1 ) attrs0
            in
            ( CA.TypeRecord pos et attrs
            , d2
            )

        CA.TypeAlias pos path t ->
            normalizeType dict t
                |> Tuple.mapFirst (CA.TypeAlias pos path)



----
--- Functions
--


functions : Test
functions =
    Test.Group "functions"
        [ simpleTest
            { name = "Known function with correct params"
            , run = \_ -> infer "a" "a = add 3 1"
            , expected = Ok { type_ = tyNumber, forall = Set.empty, mutable = False }
            }
        , hasError
            { name = "Known function with wrong params"
            , run = \_ -> infer "a" "a = add False"
            , test = Test.errorShouldContain "SPCore.Bool"
            }
        , simpleTest
            { name = "Function inference 1"
            , run = \_ -> infer "a" "a x = add x 1"
            , expected =
                Ok
                    { type_ = function tyNumber tyNumber
                    , forall = Set.empty
                    , mutable = False
                    }
            }
        , simpleTest
            { name = "Function inference 2: same as 1, but with swapped args"
            , run = \_ -> infer "a" "a x = add 1 x"
            , expected =
                Ok
                    { type_ = function tyNumber tyNumber
                    , forall = Set.empty
                    , mutable = False
                    }
            }
        , hasError
            { name = "Function args can't shadow other names"
            , run = \_ -> infer "a" "a = fn a = 1"
            , test = Test.errorShouldContain "function parameter `a` shadows env variable"
            }
            |> Test.NotNow
        , simpleTest
            { name = "[reg] fn has type None"
            , run = \_ -> infer "a" "a = fn x = 1"
            , expected =
                Ok
                    { forall = Set.fromList [ "1" ]
                    , mutable = False
                    , type_ =
                        typeFunction
                            { from = typeVariable { name = "1" }
                            , fromIsMutable = False
                            , to = typeConstant { ref = "SPCore.Number", args = [] }
                            }
                    }
            }

        --
        , codeTest "[reg] Multiple arguments are correctly inferred"
            """
            a x y z = x + y + z
            """
            (infer "a")
            Test.justOk

        --
        , codeTest "Annotation should be consistent with mutability"
            """
            f : Number @> Number
            f a = a
            """
            (infer "f")
            (Test.errContain "mutability")
        ]



----
--- Statements
--


statements : Test
statements =
    Test.Group "statements"
        [ simpleTest
            { name = "Statement blocks should return the last statement's type"
            , run =
                \_ ->
                    infer "a"
                        """
                a =
                  3
                  False
                """
            , expected = Ok { type_ = constant "SPCore.Bool", forall = Set.empty, mutable = False }
            }
        , simpleTest
            { name = "Definition statement return type None"
            , run =
                \_ ->
                    infer "a"
                        """
                a =
                  f x = 3
                """
            , expected = Ok { type_ = tyNone, forall = Set.empty, mutable = False }
            }
        , codeTest "Local values can't shadow root values"
            """
            a = 1
            b =
              a = 1
              a
            """
            (infer "b")
            (Test.errContain "already")
        , codeTest "Prevent local redeclarations"
            """
            b =
              a = 1
              a = 1
            """
            (infer "b")
            (Test.errContain "declar")
        , codeTest "Prevent root redeclarations"
            """
            a = 1
            a = 1
            """
            (infer "b")
            (Test.errContain "declar")
        ]



----
--- Variable types
--


variableTypes : Test
variableTypes =
    Test.Group "variable types"
        [ simpleTest
            { name = "Identity"
            , run =
                \_ ->
                    infer "id"
                        """
                        id : a -> a
                        id a = a
                        """
            , expected =
                Ok
                    { type_ =
                        typeFunction
                            { from = typeVariable { name = "a" }
                            , fromIsMutable = False
                            , to = typeVariable { name = "a" }
                            }
                    , forall = Set.singleton "a"
                    , mutable = False
                    }
            }
        , simpleTest
            { name = "Identity, no annotation"
            , run =
                \_ ->
                    infer "id"
                        """
                        id a = a
                        """
            , expected =
                Ok
                    { type_ =
                        typeFunction
                            { from = typeVariable { name = "1" }
                            , fromIsMutable = False
                            , to = typeVariable { name = "1" }
                            }
                    , forall = Set.singleton "1"
                    , mutable = False
                    }
            }
        , hasError
            { name = "Reject disconnected forall var types?"
            , run =
                \_ ->
                    infer "id"
                        """
                        id : a -> b
                        id l = l
                        """
            , test = Test.errorShouldContain "too general"
            }
        , isOk
            { name = "TyVar definitions: lambda scope"
            , run =
                \_ ->
                    infer "a"
                        """
                a b =
                  f x = x
                  f 3
                  f False
                """
            }
        , isOk
            { name = "TyVar definitions: non-lambda scope"
            , run =
                \_ ->
                    infer "a"
                        """
                a =
                  f x = x
                  f 3
                  f False
                """
            }
        , isOk
            { name = "TyVar definitions: root scope"
            , run =
                \_ ->
                    infer "a"
                        """
                        a x = x
                        g =
                          a 3
                          a False
                        """
            }

        {- OBSOLETE

           Since now we allow mutual recursion only across root lambda definitions, these tests are obsolete.

           If at a later time we decide that non-root functions can be mutually recursive we'll possibly restore these.

           , simpleTest
               {-
                  https://stackoverflow.com/questions/900585/why-are-functions-in-ocaml-f-not-recursive-by-default/904715#904715

                  This error happens only when the identity function (`b`) follows alphabetically
                  the definition that references it.
                  Just to be sure, I've added another test below that is identical to this one
                  with the only difference that `a` is renamed to `c`, and it passes.

                  Ok, per test #3, the problem is in the statements order.
               -}
               { name = "[reg] `a` was variable type instead than number"
               , run =
                   \_ ->
                       infer "a"
                           """
                           b x = x
                           a = b 1
                           """
               , expected = Ok { type_ = tyNumber, forall = Set.empty, mutable = False }
               }
           , simpleTest
               -- See note for the test above!
               { name = "[reg] make sure that `c` works"
               , run =
                   \_ ->
                       infer "c"
                           """
                           b x = x
                           c = b 1
                           """
               , expected = Ok { type_ = tyNumber, forall = Set.empty, mutable = False }
               }
           , simpleTest
               -- See note for the test above!
               { name = "[reg] it's in the declaration order!"
               , run =
                   \_ ->
                       infer "q"
                           """
                           q =
                             a = b 1
                             b x = x
                             a
                           """
               , expected = Ok { type_ = tyNumber, forall = Set.empty, mutable = False }
               }
        -}
        -- TODO Test self recursion and mutual recursion
        , codeTest "[reg] statements, assignments, free vars"
            """
            id a = a

            x q =
                  s = id q
                  s
            """
            (infer "x")
            (Test.okEqual
                { forall = Set.fromList [ "1" ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from = typeVariable { name = "1" }
                        , fromIsMutable = False
                        , to = typeVariable { name = "1" }
                        }
                }
            )
        , codeTest "[reg] ??? TI failure"
            """
            rec acc =
                if True then
                    acc
                else
                    rec [] "" [] acc
            """
            (infer "rec")
            (Test.errContain "")
        ]



----
--- Definitions reordering
--


referencedSiblingDefs : Dict String (Set String)
referencedSiblingDefs =
    [ ( "a", [ "b", "blah" ] )
    , ( "b", [ "a", "meh" ] )

    --
    , ( "c", [ "d" ] )
    , ( "d", [ "e" ] )
    , ( "e", [ "c" ] )

    --
    , ( "f", [ "f" ] )
    , ( "g", [ "h" ] )

    --
    , ( "cc", [ "dd" ] )
    , ( "dd", [ "ee" ] )
    , ( "ee", [ "ee" ] )
    ]
        |> List.map (Tuple.mapSecond Set.fromList)
        |> Dict.fromList



----
--- Mutability
--


mutability : Test
mutability =
    Test.Group "mutability"
        [ hasError
            { name = "Statement blocks that define mutables can't return functions"
            , run =
                \_ ->
                    infer "a"
                        """
                        a =
                          x @= 1
                          fn y = y
                        """
            , test = Test.errorShouldContain "statement blocks that define mutables can't return functions"
            }

        {-
           , simpleTest
               { name = "Infer lambda arg mutability"
               , run =
                   \_ ->
                       infer "a"
                           """
                           a =
                             q x =
                               reset @x
                             q
                           """
               , expected =
                   Ok
                       { type_ = typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone }
                       , forall = Set.empty
                       , mutable = False
                       }
               }
        -}
        , hasError
            { name = "Detect mismatching annotations"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : Number -> None
                        a =
                          reset
                        """
            , test = Test.errorShouldContain "mutability clash"
            }
        , simpleTest
            { name = "Correctly unify annotation's mutability"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : Number @> None
                        a =
                          reset
                        """
            , expected =
                Ok
                    { type_ = typeFunction { from = tyNumber, fromIsMutable = True, to = tyNone }
                    , forall = Set.empty
                    , mutable = False
                    }
            }
        , hasError
            { name = "Functions can't be mutable 1"
            , run = \_ -> infer "a" "a @= fn x = x"
            , test = Test.errorShouldContain "mutable"
            }
        , simpleTest
            { name = "Functions can't be mutable 2"
            , run = \_ -> infer "a" "a f = @f := (fn x = x)"
            , expected = Err "these mutable values contain functions: f"
            }
            |> Test.NotNow
        , hasError
            { name = "Lambda argument mutability is correctly inferred"
            , run = \_ -> infer "a" "a = fn x = reset x"
            , test = Test.errorShouldContain "mutability clash"
            }
        , hasError
            { name = "*Nested* lambda argument mutability is correctly inferred"
            , run = \_ -> infer "a" "a = fn x = (fn y = reset y) x"
            , test = Test.errorShouldContain "mutability clash"
            }
        , hasError
            { name = "Functions can't be mutable (annotation)"
            , run =
                \_ ->
                    infer "a"
                        """
                        a @: Number -> Number
                        a @= add 1
                        """
            , test = Test.errorShouldContain "mutable"
            }
        , hasError
            { name = "args that are functions can't be mutable (annotation)"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : (Int -> Int) @> Int
                        a = a
                        """
            , test = Test.errorShouldContain "mutable"
            }
        ]



----
--- Higher order types
--


higherOrderTypes : Test
higherOrderTypes =
    Test.Group "higher order types"
        [ simpleTest
            { name = "Parse precedence"
            , run =
                \_ ->
                    infer "a"
                        """
                        a : List a -> List a
                        a l = l
                        """
            , expected =
                Ok
                    { type_ =
                        typeFunction
                            { from = typeConstant { args = [ typeVariable { name = "a" } ], ref = "SPCore.List" }
                            , fromIsMutable = False
                            , to = typeConstant { args = [ typeVariable { name = "a" } ], ref = "SPCore.List" }
                            }
                    , mutable = False
                    , forall = Set.singleton "a"
                    }
            }
        , simpleTest
            { name = "Union type constructors"
            , run = \_ -> infer "L" "union X a = L"
            , expected =
                Ok
                    { type_ = typeConstant { args = [ typeVariable { name = "a" } ], ref = "Test.X" }
                    , mutable = False
                    , forall = Set.singleton "a"
                    }
            }
        ]



----
--- Records
--


records : Test
records =
    Test.Group "Records"
        [ simpleTest
            { name = "Attribute access"
            , run =
                \_ ->
                    infer "a"
                        """
                        a b = b.meh.blah
                        """
            , expected =
                Ok
                    { forall = Set.fromList [ "1", "2", "3" ]
                    , mutable = False
                    , type_ =
                        typeFunction
                            { from =
                                typeRecord
                                    { attrs =
                                        Dict.singleton "meh"
                                            (typeRecord
                                                { attrs = Dict.singleton "blah" (typeVariable { name = "3" })
                                                , extensible = Just "2"
                                                }
                                            )
                                    , extensible = Just "1"
                                    }
                            , fromIsMutable = False
                            , to = typeVariable { name = "3" }
                            }
                    }
            }
        , simpleTest
            { name = "Attribute mutation"
            , run =
                \_ ->
                    infer "a"
                        """
                        a @b = @b.meh.blah += 1
                        """
            , expected =
                Ok
                    { forall = Set.fromList [ "1", "2" ]
                    , mutable = False
                    , type_ =
                        typeFunction
                            { from =
                                typeRecord
                                    { attrs =
                                        Dict.singleton "meh"
                                            (typeRecord
                                                { attrs = Dict.singleton "blah" (typeConstant { ref = "SPCore.Number", args = [] })
                                                , extensible = Just "2"
                                                }
                                            )
                                    , extensible = Just "1"
                                    }
                            , fromIsMutable = True
                            , to = typeConstant { ref = "SPCore.None", args = [] }
                            }
                    }
            }
        , isOk
            { name = "Tuple3 direct item mutability"
            , run =
                \_ ->
                    infer "x"
                        """
                        x =
                          a @= 3 & False & 2

                          @a.third += 1
                        """
            }
        , isOk
            { name = "Tuple2 direct item mutability, annotated"
            , run =
                \_ ->
                    infer "x"
                        """
                        x =
                           a @: Number & Number
                           a @= 1 & 2

                           @a.first += 1
                        """
            }
        , simpleTest
            { name = "functional update"
            , run =
                \_ ->
                    infer "a" "a b = { b with x = 1 }"
            , expected =
                let
                    re =
                        typeRecord
                            { attrs = Dict.singleton "x" (typeConstant { args = [], ref = "SPCore.Number" })
                            , extensible = Just "1"
                            }
                in
                Ok
                    { forall = Set.fromList [ "1" ]
                    , mutable = False
                    , type_ =
                        typeFunction
                            { from = re
                            , fromIsMutable = False
                            , to = re
                            }
                    }
            }
        , simpleTest
            { name = "instantiate and refine inferred records"
            , run =
                \_ ->
                    infer "c"
                        """
                        a t = { t with x = 1 }
                        c = a
                        """
            , expected =
                let
                    re =
                        typeRecord
                            { attrs = Dict.singleton "x" (typeConstant { args = [], ref = "SPCore.Number" })
                            , extensible = Just "1"
                            }
                in
                Ok
                    { forall = Set.fromList [ "1" ]
                    , mutable = False
                    , type_ =
                        typeFunction
                            { from = re
                            , fromIsMutable = False
                            , to = re
                            }
                    }
            }
        , codeTest "[reg] excessive forallness in records"
            """
            x q =
             a = q.first
             a
            """
            (infer "x")
            (Test.okEqual
                { forall = Set.fromList [ "2", "1" ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs = Dict.fromList [ ( "first", typeVariable { name = "2" } ) ]
                                , extensible = Just "1"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "2" }
                        }
                }
            )
        , codeTest "[reg] refineType when the record has a non-extensible alias"
            """
            alias A = { c : Number, d : Number }

            upd : A -> A
            upd a =
              { a with c = .c + 1 }
            """
            (infer "upd")
            Test.justOk
        ]



----
--- Patterns
--


patterns : Test
patterns =
    Test.Group "Patterns"
        [ codeTest "List unpacking"
            """
            x q =
                   [ first, second ] = q
                   first
            """
            (infer "x")
            --
            (Test.okEqual
                { forall = Set.fromList [ "1" ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeConstant
                                { args = [ typeVariable { name = "1" } ]
                                , ref = "SPCore.List"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "1" }
                        }
                }
            )
        , codeTest "Records are correctly unpacked"
            """
            x q =
                { first } = q
                first
            """
            (infer "x")
            --
            (Test.okEqual
                { forall = Set.fromList [ "2", "1" ]
                , mutable = False
                , type_ =
                    typeFunction
                        { from =
                            typeRecord
                                { attrs = Dict.fromList [ ( "first", typeVariable { name = "2" } ) ]
                                , extensible = Just "1"
                                }
                        , fromIsMutable = False
                        , to = typeVariable { name = "2" }
                        }
                }
            )
        , {- TODO
             I can't reproduce this error in the unit tests.
             Even if I copy all code verbatim here, the error does not appear.

             I can only reproduce it on the dev environment and not reliably.
             I don't fully understand what causes it.

             Still, the problem is caused at least in part by the fact that I'm not instantiating the type for type constructors when inferring patterns
             (In TypeInference.inspectPattern#CA.PatternConstructor) which is definitely something worth fixing.

             But still, I don't understand the problem enough to reproduce it reliably.
          -}
          codeTest "[rec] Constructors should instantiate their variable types"
            """
            each : List a -> (a -> b) -> None
            each ls f =
                try ls as
                    SPCore.Nil then
                        None

            result =
                  1 :: SPCore.Nil = SPCore.Nil
            """
            (infer "result")
            --
            Test.justOk
            |> Test.NotNow
        ]



----
--- Try..As
--


try_as : Test
try_as =
    Test.Group "try..as"
        [ codeTest "basic functionality"
            """
            x q =
             try q as
               True then 2
               else 3
            """
            (infer "x")
            (Test.okEqual
                { forall = Set.fromList []
                , mutable = False
                , type_ =
                    typeFunction
                        { from = typeConstant { ref = "SPCore.Bool", args = [] }
                        , fromIsMutable = False
                        , to = typeConstant { ref = "SPCore.Number", args = [] }
                        }
                }
            )

        --
        , codeTest "rejects non-matching patterns"
            """
            x q =
             try q as
               True then 2
               [] then 3
            """
            (infer "x")
            (Test.errContain "SPCore.List")

        --
        , codeTest "rejects non-matching blocks"
            """
            x q =
             try q as
               True then 2
               False then False
            """
            (infer "x")
            (Test.errContain "SPCore.Number")
        ]



----
--- if..then
--


if_then : Test
if_then =
    Test.Group "if..then"
        [ codeTest "basic functionality"
            """
            x q =
              if q then 1
              else 2
            """
            (infer "x")
            (Test.okEqual
                { forall = Set.fromList []
                , mutable = False
                , type_ =
                    typeFunction
                        { from = typeConstant { ref = "SPCore.Bool", args = [] }
                        , fromIsMutable = False
                        , to = typeConstant { ref = "SPCore.Number", args = [] }
                        }
                }
            )

        --
        , codeTest "rejects non-bool conditions"
            """
            x q =
              if 1 then 1
              else 2
            """
            (infer "x")
            (Test.errContain "SPCore.Bool")

        --
        , codeTest "rejects non-matching blocks"
            """
            x q =
              if q then 2
              else False
            """
            (infer "x")
            (Test.errContain "SPCore.Number")
        ]
