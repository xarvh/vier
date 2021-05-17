module Prelude exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import MetaFile exposing (MetaFile)
import Types.Binop as Binop exposing (Binop)
import Types.CanonicalAst as CA exposing (Type)
import Types.Meta exposing (Meta)



----
--- Default Meta
--
-- TODO move this in its own module?


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
          "path": "SPCore/Random",
          "importAs": "Random",
          "globalValues": [
          ],
          "globalTypes": [
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
--- Prelude
--


prelude : CA.AllDefs
prelude =
    Core.coreModule
        |> (\m -> Dict.foldl insertBinop m binops)
        |> (\m -> List.foldl insertFunction m functions)


insertBinop : String -> Binop -> CA.AllDefs -> CA.AllDefs
insertBinop _ b =
    { name = b.symbol
    , localName = b.symbol
    , pos = pos
    , isNative = True
    , body = []
    , maybeAnnotation = Just b.ty
    }
        |> CA.Value
        |> Dict.insert b.symbol


insertFunction : ( String, String, CA.Type ) -> CA.AllDefs -> CA.AllDefs
insertFunction ( moduleName, localName, ty ) =
    let
        name =
            moduleName ++ "." ++ localName
    in
    { name = name
    , localName = localName
    , pos = pos
    , isNative = True
    , body = []
    , maybeAnnotation = Just ty
    }
        |> CA.Value
        |> Dict.insert name


pos : CA.Pos
pos =
    { n = "prelude"
    , c = ""
    , s = -1
    , e = -1
    }


tyVar n =
    CA.TypeVariable pos n


tyFun from to =
    CA.TypeFunction pos from False to



----
--- Binops
--


binops : Dict String Binop
binops =
    [ textConcat
    , tuple
    , listCons
    , mutableAssign
    , and
    , or
    , add
    , subtract
    , multiply
    , divide
    , mutableAdd
    , mutableSubtract
    , equal
    , lesserThan
    , greaterThan
    , sendRight
    , sendLeft
    ]
        |> List.foldl (\op -> Dict.insert op.symbol op) Dict.empty


typeBinop : Bool -> CA.Type -> CA.Type -> CA.Type -> CA.Type
typeBinop mutates left right return =
    CA.TypeFunction pos
        right
        False
        (CA.TypeFunction pos
            left
            mutates
            return
        )


typeBinopUniform ty =
    typeBinop False ty ty ty



-- Misc


textConcat : Binop
textConcat =
    { symbol = ".."
    , precedence = Binop.Addittive
    , associativity = Binop.Right
    , ty = typeBinopUniform Core.textType
    }


tuple : Binop
tuple =
    { symbol = "&"
    , precedence = Binop.Tuple
    , associativity = Binop.NonAssociative
    , ty =
        Dict.empty
            |> Dict.insert "first" (tyVar "a")
            |> Dict.insert "second" (tyVar "b")
            |> CA.TypeRecord pos Nothing
            |> typeBinop False (tyVar "a") (tyVar "b")
    }


listCons : Binop
listCons =
    let
        item =
            tyVar "item"
    in
    { symbol = "::"
    , precedence = Binop.Cons
    , associativity = Binop.Right
    , ty = typeBinop False item (Core.listType item) (Core.listType item)
    }


mutableAssign : Binop
mutableAssign =
    { symbol = ":="
    , precedence = Binop.Mutop
    , associativity = Binop.Left
    , ty = typeBinop True (tyVar "a") (tyVar "a") Core.noneType
    }



-- Arithmetic


and : Binop
and =
    { symbol = "and"
    , precedence = Binop.Logical
    , associativity = Binop.Right
    , ty = typeBinopUniform Core.boolType
    }


or : Binop
or =
    { symbol = "or"
    , precedence = Binop.Logical
    , associativity = Binop.Right
    , ty = typeBinopUniform Core.boolType
    }


add : Binop
add =
    { symbol = "+"
    , precedence = Binop.Addittive
    , associativity = Binop.Left
    , ty = typeBinopUniform Core.numberType
    }


subtract : Binop
subtract =
    { symbol = "-"
    , precedence = Binop.Addittive
    , associativity = Binop.Left
    , ty = typeBinopUniform Core.numberType
    }


multiply : Binop
multiply =
    { symbol = "*"
    , precedence = Binop.Multiplicative
    , associativity = Binop.Left
    , ty = typeBinopUniform Core.numberType
    }


divide : Binop
divide =
    { symbol = "/"
    , precedence = Binop.Multiplicative
    , associativity = Binop.Left
    , ty = typeBinopUniform Core.numberType
    }


mutableAdd : Binop
mutableAdd =
    { symbol = "+="
    , precedence = Binop.Mutop
    , associativity = Binop.NonAssociative
    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    }


mutableSubtract : Binop
mutableSubtract =
    { symbol = "-="
    , precedence = Binop.Mutop
    , associativity = Binop.NonAssociative
    , ty = typeBinop True Core.numberType Core.numberType Core.noneType
    }



-- Comparison


anyNonFunction : Type
anyNonFunction =
    -- TODO we need a CA.TypeNonFunction or something
    tyVar "nonFunction"


equal : Binop
equal =
    { symbol = "=="
    , precedence = Binop.Comparison
    , associativity = Binop.Left
    , ty = typeBinop False anyNonFunction anyNonFunction Core.boolType
    }


lesserThan : Binop
lesserThan =
    { symbol = "<"
    , precedence = Binop.Comparison
    , associativity = Binop.Left
    , ty = typeBinop False anyNonFunction anyNonFunction Core.boolType
    }


greaterThan : Binop
greaterThan =
    { symbol = ">"
    , precedence = Binop.Comparison
    , associativity = Binop.Left
    , ty = typeBinop False anyNonFunction anyNonFunction Core.boolType
    }



-- Pipes


sendRight : Binop
sendRight =
    { symbol = ">>"
    , precedence = Binop.Pipe
    , associativity = Binop.Left
    , ty =
        typeBinop False
            (tyVar "a")
            (tyFun (tyVar "a") (tyVar "b"))
            (tyVar "b")
    }


sendLeft : Binop
sendLeft =
    { symbol = "<<"
    , precedence = Binop.Pipe
    , associativity = Binop.Right
    , ty =
        typeBinop False
            (tyFun (tyVar "a") (tyVar "b"))
            (tyVar "a")
            (tyVar "b")
    }



----
--- Functions
--


functions : List ( String, String, CA.Type )
functions =
    [ debugTodo
    , debugLog
    ]



-- SPCore/Debug


debugTodo : ( String, String, CA.Type )
debugTodo =
    ( "SPCore/Debug"
    , "todo"
    , tyFun Core.textType (tyVar "a")
    )


debugLog : ( String, String, CA.Type )
debugLog =
    ( "SPCore/Debug"
    , "log"
    , tyFun Core.textType
        (tyFun (tyVar "a") (tyVar "a"))
    )
