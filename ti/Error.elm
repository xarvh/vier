module Error exposing (..)

import CanonicalAst as CA exposing (Name, Pos)
import Constraint
import Type exposing (IO, Type)


{-| from Reporting/Error/Type.hs
-}
type Error
    = BadExpr CA.Pos Constraint.Category Type (Constraint.Expected Type)
    | BadPattern CA.Pos Constraint.PatternCategory Type (Constraint.PatternExpected Type)



--| InfiniteType A.Region Name.Name Type


type MaybeName
    = FuncName Name
    | CtorName Name
    | OpName Name
    | NoName


{-| this is from Type/Error.hs
-}
type Type
    = Lambda Type Type (List Type)
    | Infinite
    | Error
    | FlexVar Name
      --| FlexSuper Super Name
    | RigidVar Name
      --| RigidSuper Super Name
    | Type CA.ModuleName Name (List Type)
      --| Record (Dict Name Type) Extension
    | Unit
    | Tuple Type Type (Maybe Type)
    | Alias CA.ModuleName Name (List ( Name, Type )) Type
