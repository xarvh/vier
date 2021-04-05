module Prelude exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import MetaFile exposing (MetaFile)
import Types.CanonicalAst as CA exposing (Type)
import Types.Meta exposing (Meta)



----
--- Prelude
--


prelude : CA.AllDefs
prelude =
    [ mutableAssign

    -- arithmetic
    , add
    , subtract
    , multiply
    , divide
    , mutableAdd

    -- comparison
    , lesserThan
    , greaterThan

    -- others
    , stringConcat
    , sendRight
    , sendLeft

    -- debug
    , debugTodo
    , debugLog
    ]
        |> List.foldl (\( n, v ) -> Dict.insert n v) Core.coreModule



----
--- Meta
--


metaString =
    """
    {
      "sourceDirs": [{
        "path": "",
        "moduleExceptions": [{
          "path": "SPCore",
          "importAs": "SPCore",
          "globalValues": [
            "None",
            "True",
            "False"
          ],
          "globalTypes": [
            "None",
            "Bool",
            "Text",
            "List",
            "Number"
          ]
        }, {
          "path": "SPCore/List",
          "importAs": "List",
          "globalValues": [
          ],
          "globalTypes": [
          ]
        }, {
          "path": "SPCore/Maybe",
          "importAs": "Maybe",
          "globalValues": [
            "Just",
            "Nothing"
          ],
          "globalTypes": [
            "Maybe"
          ]
        }, {
          "path": "SPCore/Text",
          "importAs": "Text",
          "globalValues": [
          ],
          "globalTypes": [
          ]
        }, {
          "path": "SPCore/Debug",
          "importAs": "Debug",
          "globalValues": [
            "log",
            "todo"
          ],
          "globalTypes": [
          ]
        }]
      }],
      "libraries": []
    }
    """


metaFile : MetaFile
metaFile =
    case MetaFile.stringToMetaFile metaString of
        Ok f ->
            f

        Err error ->
            Debug.todo <| "Prelude MetaFile error: " ++ error


meta : Meta
meta =
    MetaFile.toMeta metaFile



----
---
--


pos : CA.Pos
pos =
    { n = "prelude"
    , c = ""
    , s = -1
    , e = -1
    }


tyVar n =
    CA.TypeVariable pos n


constant n =
    CA.TypeConstant pos n []


function from to =
    CA.TypeFunction pos from Nothing to



----
--- Binops
--


type alias NativeBinopArgs =
    { symbol : String
    , left : CA.Type
    , right : CA.Type
    , return : CA.Type
    , mutates : Bool
    }


nativeBinop : NativeBinopArgs -> ( String, CA.RootDef )
nativeBinop ar =
    ( ar.symbol
    , CA.Value
        { pattern = CA.PatternAny pos ar.symbol
        , mutable = False
        , body = []
        , maybeAnnotation =
            Just
                (CA.TypeFunction pos
                    ar.left
                    (Just False)
                    (CA.TypeFunction pos
                        ar.right
                        (Just ar.mutates)
                        ar.return
                    )
                )
        }
    )


mutableAssign : ( String, CA.RootDef )
mutableAssign =
    nativeBinop
        { symbol = ":="
        , left = CA.TypeVariable pos "a"
        , right = CA.TypeVariable pos "a"
        , return = Core.noneType
        , mutates = True
        }



-- arithmetic


add : ( String, CA.RootDef )
add =
    nativeBinop
        { symbol = "+"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


subtract : ( String, CA.RootDef )
subtract =
    nativeBinop
        { symbol = "-"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


multiply : ( String, CA.RootDef )
multiply =
    nativeBinop
        { symbol = "*"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


divide : ( String, CA.RootDef )
divide =
    nativeBinop
        { symbol = "/"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.numberType
        , mutates = False
        }


mutableAdd : ( String, CA.RootDef )
mutableAdd =
    nativeBinop
        { symbol = "+="
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.noneType
        , mutates = True
        }



-- Comparison binops


{-| TODO I don't have a `comparable` typeclass, how do I give a type to these?
-}
lesserThan : ( String, CA.RootDef )
lesserThan =
    nativeBinop
        { symbol = "<"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.boolType
        , mutates = False
        }


greaterThan : ( String, CA.RootDef )
greaterThan =
    nativeBinop
        { symbol = ">"
        , left = Core.numberType
        , right = Core.numberType
        , return = Core.boolType
        , mutates = False
        }



-- Other binops


stringConcat : ( String, CA.RootDef )
stringConcat =
    nativeBinop
        { symbol = ".."
        , left = Core.textType
        , right = Core.textType
        , return = Core.textType
        , mutates = False
        }


sendRight : ( String, CA.RootDef )
sendRight =
    nativeBinop
        { symbol = ">>"
        , left = tyVar "a"
        , right = function (tyVar "a") (tyVar "b")
        , return = tyVar "b"
        , mutates = False
        }


sendLeft : ( String, CA.RootDef )
sendLeft =
    nativeBinop
        { symbol = "<<"
        , left = function (tyVar "a") (tyVar "b")
        , right = tyVar "a"
        , return = tyVar "b"
        , mutates = False
        }



----
--- SPCore/Debug
--


debugTodo : ( String, CA.RootDef )
debugTodo =
    ( "SPCore/Debug.todo"
    , CA.Value
        { pattern = CA.PatternAny pos "SPCore/Debug.todo"
        , mutable = False
        , body = [{- TODO -}]
        , maybeAnnotation =
            Just
                (CA.TypeFunction pos
                    Core.textType
                    (Just False)
                    (CA.TypeVariable pos "a")
                )
        }
    )


debugLog : ( String, CA.RootDef )
debugLog =
    ( "SPCore/Debug.log"
    , CA.Value
        { pattern = CA.PatternAny pos "SPCore/Debug.log"
        , mutable = False
        , body = [{- TODO -}]
        , maybeAnnotation =
            Just
                (CA.TypeFunction pos
                    Core.textType
                    (Just False)
                    (CA.TypeFunction pos
                        (CA.TypeVariable pos "a")
                        (Just False)
                        (CA.TypeVariable pos "a")
                    )
                )
        }
    )
