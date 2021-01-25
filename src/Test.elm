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
    = Single (() -> TestOutcome)
    | Group String (List Test)


type alias TestOutcome =
    { name : String
    , maybeError : Maybe String
    }



----
--- Constructors
--


type CodeExpectation ok
    = CodeExpectation ((ok -> String) -> Result String ok -> Maybe String)


codeTest : (ok -> String) -> String -> String -> (String -> Result String ok) -> CodeExpectation ok -> Test
codeTest toString title code functionToTest (CodeExpectation toMaybeError) =
    Single <| \() ->
    { name = title ++ "\n\n" ++ code
    , maybeError = code |> functionToTest |> toMaybeError toString
    }


okEqual : ok -> CodeExpectation ok
okEqual expectedOk =
    CodeExpectation <| \toString result ->
    case result of
        Err e ->
            Just e

        Ok actualOk ->
            if expectedOk == actualOk then
                Nothing

            else
                [ "expected = "
                , toString expectedOk
                , "actual = "
                , toString actualOk
                ]
                    |> String.join "\n"
                    |> Just


simple :
    (outcome -> String)
    ->
        { name : String
        , run : String -> outcome
        , expected : outcome
        }
    -> Test
simple toString { name, run, expected } =
    Single
        (\() ->
            { name = name
            , maybeError =
                let
                    actual =
                        run name
                in
                if actual == expected then
                    Nothing

                else
                    [ "Expected: "
                    , toString expected
                    , "\n"
                    , "Actual: "
                    , toString actual
                    ]
                        |> String.join ""
                        |> Just
            }
        )


isOk :
    (error -> String)
    ->
        { name : String
        , run : String -> Result error outcome
        }
    -> Test
isOk toString { name, run } =
    Single
        (\() ->
            { name = name
            , maybeError =
                case run name of
                    Ok _ ->
                        Nothing

                    Err e ->
                        Just <| "Error: " ++ toString e
            }
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
    Single
        (\() ->
            { name = name
            , maybeError =
                case run name of
                    Ok outcome ->
                        Just <| "Ok: " ++ toString outcome

                    Err e ->
                        test e
            }
        )


errorShouldContain : String -> String -> Maybe String
errorShouldContain s error =
    if String.contains s error then
        Nothing

    else
        Just <| "Error \"" ++ error ++ "\" should contain \"" ++ s ++ "\""



----
--- View
--


outcomesRec : String -> Test -> List TestOutcome -> List TestOutcome
outcomesRec path t accum =
    case t of
        Single f ->
            let
                outcome =
                    f ()
            in
            { outcome | name = path ++ outcome.name } :: accum

        Group pathSegment ts ->
            List.foldl (outcomesRec (path ++ pathSegment ++ " / ")) accum ts


viewList : List Test -> Html msg
viewList tests =
    tests
        |> List.foldl (outcomesRec "") []
        |> List.sortBy
            (\t ->
                if t.maybeError /= Nothing then
                    0

                else
                    1
            )
        |> List.map view
        |> (::) style
        |> Html.div []


view : TestOutcome -> Html msg
view test =
    Html.div
        [ class "test-item"
        , if test.maybeError == Nothing then
            class "test-ok"

          else
            class "test-error"
        ]
        [ Html.pre [ class "test-name" ] [ Html.text test.name ]
        , Html.code []
            [ case test.maybeError of
                Nothing ->
                    Html.text "Ok!"

                Just error ->
                    error
                        |> String.split "\n"
                        |> List.map (\e -> Html.div [ class "test-error-line" ] [ Html.text e ])
                        |> Html.div []
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
}

.test-name {
  margin-bottom: 0.5em;
}

.test-ok {
  background-color: #6f6;
}

.test-error {
  background-color: #f66;
}

.test-error-line {
  margin-bottom: 0.5em;
}
  """
        ]
