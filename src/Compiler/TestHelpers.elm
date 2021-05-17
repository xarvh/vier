module Compiler.TestHelpers exposing (..)

import Compiler.ApplyAliases
import Compiler.Pipeline
import Dict exposing (Dict)
import Prelude exposing (meta)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Res)
import Types.FormattableAst as FA


moduleName =
    "Test"


p : CA.Pos
p =
    { n = "th"
    , c = ""
    , s = 0
    , e = 0
    }


type alias CA_Fold_Function target acc =
    (CA.Fold -> ( CA.Pos, acc ) -> ( CA.Pos, acc )) -> ( target, acc ) -> ( target, acc )


removePos : CA_Fold_Function target () -> target -> target
removePos fold_helper target =
    ( target, () )
        |> fold_helper (\_ _ -> ( p, () ))
        |> Tuple.first


resErrorToString : Res a -> Result String a
resErrorToString =
    Result.mapError (\e -> Error.flatten e [] |> List.map Error.toString |> String.join "\n\n")


stringToCanonicalModuleWithPos : String -> Res CA.AllDefs
stringToCanonicalModuleWithPos code =
    code
        |> unindent
        |> (\c -> Compiler.Pipeline.stringToCanonicalAst meta moduleName c)
        |> Result.map (Dict.union Prelude.prelude)
        |> Result.andThen Compiler.ApplyAliases.applyAliasesToModule


stringToCanonicalModule : String -> Res CA.AllDefs
stringToCanonicalModule code =
    code
        |> stringToCanonicalModuleWithPos
        |> Result.map (removePos CA.extensionFold_module)


stringToFormattableModule : String -> Res FA.Module
stringToFormattableModule code =
    code
        |> unindent
        |> Compiler.Pipeline.stringToFormattableAst moduleName


unindent : String -> String
unindent multilineString =
    let
        lines =
            String.lines multilineString

        countLeadingSpaces line =
            case String.uncons line of
                Nothing ->
                    0

                Just ( char, xs ) ->
                    case char of
                        ' ' ->
                            1 + countLeadingSpaces xs

                        _ ->
                            0

        minLead =
            lines
                |> List.filter (String.any ((/=) ' '))
                |> List.map countLeadingSpaces
                |> List.minimum
                |> Maybe.withDefault 0
    in
    lines
        |> List.map (String.dropLeft minLead)
        |> String.join "\n"
