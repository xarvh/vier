module Type exposing (..)

import CanonicalAst as CA exposing (Name, Pos)
import IO exposing (IO)



-- type RigidTypeVars
--     = Dict Name Type


type alias Variable =
    Int


{-| TODO why do we need both this and CA.Type?

         = PlaceHolder Name.Name
         | AliasN ModuleName.Canonical Name.Name [(Name.Name, Type)] Type
         | EmptyRecordN
         | RecordN (Map.Map Name.Name Type) Type
         | UnitN
         | TupleN Type Type (Maybe Type)

-}
type Type
    = VarN CA.TyVarRef
    | AppN CA.ModuleName Name (List Type)
    | FunN Type Type



-- type variable generators


mkFlexVar : IO Variable
mkFlexVar =
    IO.newId

nameToFlex : Name -> IO Variable
nameToFlex name =
    -- TODO
    IO.newId
