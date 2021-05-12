module Compiler.ApplyAliases_Test exposing (..)

import Compiler.ApplyAliases
import Compiler.TestHelpers as TH exposing (p)
import Dict exposing (Dict)
import Set exposing (Set)
import Test exposing (Test)
import Types.CanonicalAst as CA


isOk =
    Test.isOk Debug.toString


simpleTest =
    Test.simple Debug.toString


hasError =
    Test.hasError Debug.toString


applyAndGet : (CA.RootDef -> Maybe a) -> String -> String -> Result String a
applyAndGet getAs name code =
    code
        |> TH.stringToCanonicalModule
        |> TH.resErrorToString
        |> Result.andThen (Dict.get ("Test." ++ name) >> Result.fromMaybe "dict error!")
        |> Result.andThen (getAs >> Result.fromMaybe "wrong variant")


applyAndGetValue : String -> String -> Result String CA.RootValueDef
applyAndGetValue name code =
    code
        |> TH.stringToCanonicalModule
        |> TH.resErrorToString
        |> Result.andThen (CA.findValue ("Test." ++ name) >> Result.fromMaybe "findValue error!")



----
---
--


tests : Test
tests =
    Test.Group "ApplyAliases"
        [ Test.Group "annotations"
            [ simpleTest
                { name = "simple"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        a : A Number Bool
                        a = a
                        """
                            |> applyAndGetValue "a"
                            |> Result.map .maybeAnnotation
                , expected =
                    [ CA.TypeConstant p "SPCore.Number" [] ]
                        |> CA.TypeConstant p "SPCore.List"
                        |> CA.TypeAlias p "Test.A"
                        |> Just
                        |> Ok
                }
            , hasError
                { name = "Reject wrong number of args"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        a : A Bool
                        a = a
                        """
                            |> applyAndGetValue "a"
                , test = Test.errorShouldContain "alias Test.A needs 2 args, but was used with 1"
                }
            , simpleTest
                { name = "record"
                , run =
                    \_ ->
                        """
                        alias A b = { x : b, y : b }
                        a : A Bool
                        a = a
                        """
                            |> applyAndGetValue "a"
                            |> Result.map .maybeAnnotation
                , expected =
                    Dict.empty
                        |> Dict.insert "x" (CA.TypeConstant p "SPCore.Bool" [])
                        |> Dict.insert "y" (CA.TypeConstant p "SPCore.Bool" [])
                        |> CA.TypeRecord p Nothing
                        |> CA.TypeAlias p "Test.A"
                        |> Just
                        |> Ok
                }
            ]
        , Test.Group "unions"
            [ simpleTest
                { name = "simple"
                , run =
                    \_ ->
                        """
                        alias A b c = List b
                        union B x = B1 (A Bool x)
                        """
                            |> applyAndGet CA.asUnion "B"
                            |> Result.map .constructors
                , expected =
                    Ok <|
                        Dict.singleton "Test.B1"
                            [ CA.TypeAlias p
                                "Test.A"
                                (CA.TypeConstant p
                                    "SPCore.List"
                                    [ CA.TypeConstant p "SPCore.Bool" []
                                    ]
                                )
                            ]
                }
            ]

        {-
           , Test.Group "aliases"
               [ simpleTest
                   { name = "simple"
                   , run =
                       \_ ->
                           """
                           alias A b c = List b
                           alias B x = A Bool x
                           """
                               |> applyAndGet CA.asAlias "B"
                               |> Result.map .ty
                   , expected =
                       Ok <| CA.TypeAlias "Test.A" (CA.TypeConstant { ref = "SPCore.List", args = [ CA.TypeConstant { ref = "SPCore.Bool", args = [] } ] })
                   }
               , hasError
                   { name = "reject circular aliases"
                   , run =
                       \_ ->
                           """
                           alias A = B -> B
                           alias B = [ A ]
                           """
                               |> applyAndGet CA.asAlias "B"
                   , test =
                       Test.errorShouldContain "circular"
                   }
               ]
        -}
        ]
