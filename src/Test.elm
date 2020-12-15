module Test exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)


type alias Test =
    { name : String
    , maybeError : Maybe String
    }



----
--- Constructors
--


simple :
    (outcome -> String)
    ->
        { name : String
        , run : () -> outcome
        , expected : outcome
        }
    -> Test
simple toString { name, run, expected } =
    { name = name
    , maybeError =
        let
            actual =
                run ()
        in
        if actual == expected then
            Nothing

        else
            [ "Expected: "
            , toString expected
            , "Actual: "
            , toString actual
            ]
                |> String.join "\n"
                |> Just
    }


isOk :
    (error -> String)
    ->
        { name : String
        , run : () -> Result error outcome
        }
    -> Test
isOk toString { name, run } =
    { name = name
    , maybeError =
        case run () of
            Ok _ ->
                Nothing

            Err e ->
                Just <| "Error: " ++ toString e
    }



----
--- View
--


viewList : List Test -> Html msg
viewList tests =
    tests
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


view : Test -> Html msg
view test =
    Html.div
        [ class "test-item"
        , if test.maybeError == Nothing then
            class "test-ok"

          else
            class "test-error"
        ]
        [ Html.div [ class "test-name" ] [ Html.text test.name ]
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
