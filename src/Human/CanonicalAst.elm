module Human.CanonicalAst exposing (..)

import Dict exposing (Dict)
import Types.CanonicalAst as CA exposing (Type)


typeToString : Type -> String
typeToString =
    typeToPriAndString >> Tuple.second


parensIf : Bool -> String -> String
parensIf test s =
    if test then
        "(" ++ s ++ ")"

    else
        s


parensIfGreaterThan : Int -> Type -> String
parensIfGreaterThan threshold ty =
    let
        ( pri, str ) =
            typeToPriAndString ty
    in
    parensIf (pri > threshold) str


typeToPriAndString : CA.Type -> ( Int, String )
typeToPriAndString type_ =
    case type_ of
        CA.TypeConstant pos name args ->
            ( if args == [] then
                0

              else
                1
            , name
                :: List.map (parensIfGreaterThan 0) args
                |> String.join " "
            )

        CA.TypeVariable pos name ->
            ( 0
            , name
            )

        CA.TypeFunction pos from fromIsMut to ->
            let
                arrow =
                    case fromIsMut of
                        Nothing ->
                            "?>"

                        Just True ->
                            "@>"

                        Just False ->
                            "->"
            in
            ( 2
            , [ parensIfGreaterThan 1 from
              , arrow
              , parensIfGreaterThan 2 to
              ]
                |> String.join " "
            )

        CA.TypeRecord pos extend attrs ->
            let
                attrsString =
                    attrs
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map (\( name, ty ) -> name ++ " : " ++ typeToString ty)
                        |> String.join ", "
            in
            ( 0
            , [ "{ "
              , case extend of
                    Nothing ->
                        ""

                    Just n ->
                        n ++ " with"
              , attrsString
              , " }"
              ]
                |> String.join ""
            )

        CA.TypeAlias pos name ty2 ->
            ( 0
            , [ "<"
              , name
              , "="
              , typeToString ty2
              , ">"
              ]
                |> String.join " "
            )
