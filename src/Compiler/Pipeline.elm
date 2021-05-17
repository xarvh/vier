module Compiler.Pipeline exposing (..)

import Compiler.FormattableToCanonicalAst
import Compiler.ScopeCheck
import Compiler.StringToTokens
import Compiler.TokensToFormattableAst
import Dict exposing (Dict)
import Types.CanonicalAst as CA
import Types.Error exposing (Res)
import Types.FormattableAst as FA
import Types.Meta exposing (Meta)
import Types.Token exposing (Token)



----
--- Single Modules
--

andThenMapError : (e -> f) -> (a -> Result e b) -> Result f a -> Result f b
andThenMapError transformError f =
    Result.andThen (f >> Result.mapError transformError)


stringToTokens : String -> String -> Res (List Token)
stringToTokens moduleName code =
    code
        |> Compiler.StringToTokens.lexer


stringToFormattableAst : String -> String -> Res FA.Module
stringToFormattableAst moduleName code =
    code
        |> stringToTokens moduleName
        |> Result.andThen (Compiler.TokensToFormattableAst.parse moduleName code)


stringToCanonicalAst : Meta -> String -> String -> Res CA.AllDefs
stringToCanonicalAst meta moduleName code =
    let
        ro =
            { meta = meta
            , code = code
            , currentModule = moduleName
            }
    in
    code
        |> stringToFormattableAst moduleName
        |> Result.andThen (\fa -> Compiler.FormattableToCanonicalAst.translateModule ro fa Dict.empty)
        |> Result.andThen (\ca -> Compiler.ScopeCheck.onModule meta ca |> Result.map (always ca))



{-

   toTokens : String -> Res (List Token)


   toFormattableAst : String -> String -> { tokens : Res (List Token), faModule : FA.Module }


   toCanonicalAst : Meta -> String -> String -> Res { tokens : List Token, faModule : FA.Module, caModule : CA.AllDefs }


   type alias ModulePipeline =
       { tokens : Maybe (Res (List Token))
       , faModule : Maybe (Res FA.Module)
       , caModule : Maybe (Res CA.Module)
       }
-}
