module Test exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)



{- TODO

   A nicer API would allow to write names and use them as group names or test names

      test =
          simple "multiline comments"
              {...
              }

      test =
          group "multiline comments"
               [...
               ]

-}


type Test
    = Single String (() -> TestOutcome)
    | Group String (List Test)
    | NotNow Test


type TestOutcome
    = Success
    | Skipped
    | Error String


maybeToOutcome : Maybe String -> TestOutcome
maybeToOutcome m =
    case m of
        Just e ->
            Error e

        Nothing ->
            Success



----
--- Constructors (code)
--


type CodeExpectation ok
    = CodeExpectation ((ok -> String) -> Result String ok -> Maybe String)


codeTest : (ok -> String) -> String -> String -> (String -> Result String ok) -> CodeExpectation ok -> Test
codeTest toString title code functionToTest (CodeExpectation toMaybeError) =
    Single (title ++ "\n\n" ++ code) <| \() ->
    code
        |> functionToTest
        |> toMaybeError toString
        |> maybeToOutcome


{-| TODO remove old-system isOk below and rename this to isOk
-}
justOk : CodeExpectation ok
justOk =
    CodeExpectation <| \toString result ->
    case result of
        Err e ->
            Just e

        Ok actualOk ->
            Nothing

freeform : (ok -> Maybe String) -> CodeExpectation ok
freeform test =
    CodeExpectation <| \toString result ->
    case result of
        Err e ->
            Just e

        Ok actualOk ->
          test actualOk

okEqual : ok -> CodeExpectation ok
okEqual expectedOk =
    CodeExpectation <| \toString result ->
    case result of
        Err e ->
            Just e

        {-
           [ "expected = "
           , "  " ++ toString expectedOk
           , ""
           , "Err "
           , "  " ++ e
           ]
               |> String.join "\n"
               |> Just
        -}
        Ok actualOk ->
            if expectedOk == actualOk then
                Nothing

            else
                [ "expected = "
                , toString expectedOk
                , ""
                , "actual = "
                , toString actualOk
                ]
                    |> String.join "\n"
                    |> Just


errContain : String -> CodeExpectation ok
errContain subString =
    CodeExpectation <| \toString result ->
    case result of
        Err error ->
            if String.contains subString error then
                Nothing

            else
                [ "error should contain \"" ++ subString ++ "\""
                , "but instead is:"
                , ""
                , error
                ]
                    |> String.join "\n"
                    |> Just

        Ok ok ->
            [ "expecting an error containing \"" ++ subString ++ "\" but instead got Ok!"
            , ""
            , toString ok
            ]
                |> String.join "\n"
                |> Just



----
--- Constructors
--


simple :
    (outcome -> String)
    ->
        { name : String
        , run : String -> outcome
        , expected : outcome
        }
    -> Test
simple toString { name, run, expected } =
    Single name
        (\() ->
            let
                actual =
                    run name
            in
            if actual == expected then
                Success

            else
                [ "Expected: "
                , toString expected
                , "\n"
                , "Actual  : "
                , toString actual
                ]
                    |> String.join ""
                    |> Error
        )


isOk :
    (error -> String)
    ->
        { name : String
        , run : String -> Result error outcome
        }
    -> Test
isOk toString { name, run } =
    Single name
        (\() ->
            case run name of
                Ok _ ->
                    Success

                Err e ->
                    Error <| toString e
        )


hasError :
    (outcome -> String)
    ->
        { name : String
        , run : String -> Result error outcome
        , test : error -> Maybe String
        }
    -> Test
hasError toString { name, run, test } =
    Single name
        (\() ->
            case run name of
                Ok outcome ->
                    Error <| "Ok: " ++ toString outcome

                Err e ->
                    maybeToOutcome <| test e
        )


errorShouldContain : String -> String -> Maybe String
errorShouldContain s error =
    if String.contains s error then
        Nothing

    else
        Just <| "\nThe error... \n\n" ++ error ++ "\n\n...should contain \"" ++ s ++ "\""



----
--- View
--


outcomesRec : String -> Test -> List ( String, TestOutcome ) -> List ( String, TestOutcome )
outcomesRec path t accum =
    case t of
        Single name f ->
            ( path ++ name, f () ) :: accum

        NotNow test ->
            ( path ++ getName test, Skipped ) :: accum

        Group pathSegment ts ->
            List.foldl (outcomesRec (path ++ pathSegment ++ " / ")) accum ts


getName : Test -> String
getName test =
    case test of
        Single n f ->
            n

        Group n ls ->
            n

        NotNow t ->
            getName t


viewList : List Test -> Html msg
viewList tests =
    tests
        |> List.foldl (outcomesRec "") []
        |> List.sortBy
            (\( name, outcome ) ->
                case outcome of
                    Error e ->
                        -1

                    Skipped ->
                        0

                    Success ->
                        1
            )
        |> List.map view
        |> (::) style
        |> Html.div []


view : ( String, TestOutcome ) -> Html msg
view ( name, outcome ) =
    Html.div
        [ class "test-item"
        , case outcome of
            Success ->
                class "test-ok"

            Skipped ->
                class "test-skipped"

            Error e ->
                class "test-error"
        ]
        [ Html.pre [ class "test-name" ] [ Html.text name ]
        , Html.code []
            [ case outcome of
                Success ->
                    Html.text "Ok!"

                Skipped ->
                    Html.text "Skipped!"

                Error error ->
                    Html.code
                        []
                        [ Html.pre
                            []
                            [ Html.text error ]
                        ]
            ]
        ]


style : Html msg
style =
    Html.node "style"
        []
        [ Html.text
            """
.test-item {
  padding: 1em;
  margin-bottom: 2px;
  color: #222;
}

.test-name {
  margin-bottom: 0.5em;
}

.test-ok {
  background-color: #6f6;
}

.test-skipped {
  background-color: #fe3;
}

.test-error {
  background-color: #f66;
}

.test-error-line {
  margin-bottom: 0.5em;
}
  """
        ]
