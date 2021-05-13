module Compiler.FormattableToCanonicalAst exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Lib
import SepList exposing (SepList)
import Set exposing (Set)
import Types.Binop as Binop exposing (Binop)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Res, errorTodo)
import Types.FormattableAst as FA
import Types.Meta exposing (Meta)
import Types.Token as Token


{-| `maybeUpdateTarget` is used for record update shorthands:

      new = { old | x = .x + 1 }

    `nonRootValues` is used to keep track of value that are declared within the
    scope of a function, so they don't need to be expanded with the module name.

    `meta` is read-only stuff, used to expand names.

-}
type alias Env =
    { maybeUpdateTarget : Maybe CA.VariableArgs
    , nonRootValues : Set String
    , ro : ReadOnly
    }


type alias ReadOnly =
    { meta : Meta
    , code : String
    , currentModule : String
    }


initEnv : ReadOnly -> Env
initEnv ro =
    { maybeUpdateTarget = Nothing
    , nonRootValues = Set.empty
    , ro = ro
    }


do a b =
    Result.andThen b a



----
--- Position
--


todoPos : CA.Pos
todoPos =
    { n = "TODO"
    , c = ""
    , s = -1
    , e = -1
    }


{-| translate position
-}
tp : ReadOnly -> FA.Pos -> CA.Pos
tp ro ( start, end ) =
    { n = ro.currentModule
    , c = ro.code
    , s = start
    , e = end
    }



----
--- Module
--


translateModule : ReadOnly -> FA.Module -> CA.AllDefs -> Res CA.AllDefs
translateModule ro faModule caModule =
    case faModule of
        [] ->
            Ok caModule

        faStat :: faStatTail ->
            caModule
                |> insertRootStatement ro faStat
                |> Result.andThen (translateModule ro faStatTail)


insertRootStatement : ReadOnly -> FA.Statement -> CA.AllDefs -> Res CA.AllDefs
insertRootStatement ro faStatement caModule =
    case faStatement of
        FA.Evaluation _ ->
            errorTodo "Root Evaluations don't really do much =|"

        FA.Definition fa ->
            let
                env =
                    initEnv ro
            in
            do (translateDefinition True env fa) <| \localDef ->
            if localDef.mutable then
                errorRootDefinitionsCantBeMutable env localDef

            else
                case localDef.pattern of
                    CA.PatternAny caPos defName ->
                        let
                            caName =
                                makeRootName ro.currentModule defName

                            rootDef =
                                { name = caName
                                , pos = caPos
                                , maybeAnnotation = localDef.maybeAnnotation
                                , isNative = False
                                , body = localDef.body
                                }
                        in
                        if Dict.member caName caModule then
                            errorTodo <| defName ++ " declared twice!"

                        else
                            caModule
                                |> Dict.insert caName (CA.Value rootDef)
                                |> Ok

                    _ ->
                        errorTodo "patterns can't be used in root definitions!"

        FA.TypeAlias fa ->
            let
                caName =
                    makeRootName ro.currentModule fa.name
            in
            if Dict.member caName caModule then
                errorTodo <| fa.name ++ " declared twice!"

            else if not <| startsWithUpperChar fa.name then
                errorTodo "type name should be uppercase"

            else
                do (translateType ro fa.ty) <| \caType ->
                caModule
                    |> Dict.insert caName
                        (CA.Alias
                            { name = caName
                            , args = fa.args
                            , ty = caType
                            }
                        )
                    |> Ok

        FA.UnionDef fa ->
            let
                caName =
                    makeRootName ro.currentModule fa.name
            in
            if Dict.member caName caModule then
                errorTodo <| fa.name ++ " declared twice!"

            else if not <| startsWithUpperChar fa.name then
                errorTodo "type name should be uppercase"

            else
                do (Lib.list_foldlRes (translateConstructor ro) fa.constructors Dict.empty) <| \constructors ->
                caModule
                    |> Dict.insert caName
                        (CA.Union
                            { name = caName
                            , args = fa.args
                            , constructors = constructors
                            }
                        )
                    |> Ok


translateConstructor : ReadOnly -> FA.Type -> Dict String (List CA.Type) -> Res (Dict String (List CA.Type))
translateConstructor ro faType constructors =
    case faType of
        FA.TypeName pos name ->
            translateConstructor ro (FA.TypePolymorphic pos name []) constructors

        FA.TypePolymorphic pos fa_name fa_args ->
            do (stringToStructuredName (initEnv ro) pos fa_name) <| \sname ->
            case sname of
                StructuredName_Value _ ->
                    errorTodo "constructor name must start with a uppercase letter"

                StructuredName_TypeOrCons consArgs ->
                    if consArgs.mod /= NotSpecified then
                        errorTodo "something's wrong with  the cons name"

                    else
                        let
                            name =
                                makeRootName ro.currentModule consArgs.name
                        in
                        if Dict.member name constructors then
                            -- TODO prevent different types from having constructors of the same name!
                            errorTodo <| "constructor " ++ name ++ " is duplicate"

                        else
                            do (Lib.list_mapRes (translateType ro) fa_args) <| \caArgs ->
                            constructors
                                |> Dict.insert name caArgs
                                |> Ok

        _ ->
            errorTodo "either this constructor does not start with a name, either there's something off with the operators"



----
--- Structured Names
--


type StructuredName
    = StructuredName_Value
        { name : String
        , mod : Mod
        , attrPath : List String
        }
    | StructuredName_TypeOrCons
        { name : String
        , mod : Mod
        }


type Mod
    = NotSpecified
    | AlreadyEmbedded
    | ResolvedTo String


resolveName : (Meta -> Dict String String) -> ReadOnly -> Bool -> Mod -> String -> String
resolveName getter ro declaredInsideFunction mod name =
    case mod of
        ResolvedTo modName ->
            makeRootName modName name

        AlreadyEmbedded ->
            name

        NotSpecified ->
            case Dict.get name (getter ro.meta) of
                Just global ->
                    global

                Nothing ->
                    if declaredInsideFunction then
                        name

                    else
                        makeRootName ro.currentModule name


resolveValueName : ReadOnly -> Bool -> Mod -> String -> String
resolveValueName =
    resolveName .globalValues


resolveTypeName : ReadOnly -> Bool -> Mod -> String -> String
resolveTypeName =
    resolveName .globalTypes


makeRootName : String -> String -> String
makeRootName modName defName =
    modName ++ "." ++ defName


stringToStructuredName : Env -> FA.Pos -> String -> Res StructuredName
stringToStructuredName env pos rawString =
    {-
       .attr
       .attr1.attr2

       value
       value.attr1.attr2

       Type
       Constructor

       Module.Type
       Module.Constructor
       Module.value
       Module.value.attr1.attr2

       Dir/Module.Type
       Dir/Module.Constructor
       Dir/Module.value
       Dir/Module.value.attr1.attr2
    -}
    let
        validateAttrPath : List String -> Res (List String)
        validateAttrPath ap =
            if List.any startsWithUpperChar ap then
                errorTodo "record attributes names must start with a lowercase letter"

            else if List.any (String.contains "/") ap then
                errorTodo "`/` can't be used inside attribute names"

            else if List.any ((==) "") ap then
                errorTodo "Weird `..`?"

            else
                Ok ap

        validateDefName : String -> Res String
        validateDefName name =
            -- TODO can't be ""
            if String.contains "/" name then
                errorTodo "value names can't contain `/`"

            else if name == "" then
                errorTodo "weird double dots?"

            else
                Ok name

        translateModName : String -> Res String
        translateModName moduleName =
            Dict.get moduleName env.ro.meta.bynames
                |> Maybe.withDefault moduleName
                |> Ok
    in
    case String.split "." rawString of
        [] ->
            errorTodo "name is empty string !? should not happen"

        --
        -- `.attr`
        -- `.attr1.attr2`
        --
        -- starts with `.`, so it's a record update shorthand
        --
        "" :: tail ->
            case env.maybeUpdateTarget of
                Nothing ->
                    errorRecordUpdateShorthandOutsideRecordUpdate pos rawString env

                Just ref ->
                    do (validateAttrPath tail) <| \tailPath ->
                    { name = ref.name
                    , mod = AlreadyEmbedded
                    , attrPath = ref.attrPath ++ tailPath
                    }
                        |> StructuredName_Value
                        |> Ok

        first :: rest ->
            if not <| startsWithUpperChar first then
                -- `value`
                -- `value.attr1.attr2`
                do (validateAttrPath rest) <| \attrPath ->
                do (validateDefName first) <| \name ->
                { name = name
                , mod = NotSpecified
                , attrPath = attrPath
                }
                    |> StructuredName_Value
                    |> Ok

            else
                case rest of
                    [] ->
                        -- `SomeType`
                        -- `SomeConstructor`
                        do (validateDefName first) <| \name ->
                        { name = name
                        , mod = NotSpecified
                        }
                            |> StructuredName_TypeOrCons
                            |> Ok

                    second :: tail ->
                        if startsWithUpperChar second then
                            -- `Module.Type`
                            -- `Module.Constructor`
                            if tail /= [] then
                                errorTodo "Type or Constructor can't have attributes!"

                            else
                                do (validateDefName second) <| \defName ->
                                do (translateModName first) <| \modName ->
                                { name = defName
                                , mod = ResolvedTo modName
                                }
                                    |> StructuredName_TypeOrCons
                                    |> Ok

                        else
                            -- `Module.value`
                            -- `Module.value.attr1.attr2`
                            do (validateDefName second) <| \defName ->
                            do (translateModName first) <| \modName ->
                            do (validateAttrPath tail) <| \attrPath ->
                            { name = defName
                            , mod = ResolvedTo modName
                            , attrPath = attrPath
                            }
                                |> StructuredName_Value
                                |> Ok



----
--- Definition
--
{-
   translateRootDefinition : FA.ValueDef -> Res (CA.RootDef ())
   translateRootDefinition fa =
       case fa.pattern of
           FA.PatternAny name ->
               Result.map3
                   (\maybeAnnotation body caParameters ->
                       { name = name
                       , maybeAnnotation = maybeAnnotation
                       , body = List.foldr wrapLambda body caParameters
                       }
                   )
                   (translateMaybeAnnotation fa)
                   (translateStatementBlock Nothing fa.body)
                   (Lib.list_mapRes translatePattern fa.parameters)

           _ ->
               errorTodo "Root definitions can't be patterns"
-}


insertParamNames : CA.Parameter -> Set String -> Set String
insertParamNames param =
    case param of
        CA.ParameterMutable pos n ->
            Set.insert n

        CA.ParameterPattern pa ->
            CA.patternNames pa |> Set.union


translateDefinition : Bool -> Env -> FA.ValueDef -> Res CA.LocalValueDef
translateDefinition isRoot env fa =
    do (validateFaDefinition fa) <| \_ ->
    do (translateMaybeAnnotation env.ro fa) <| \maybeAnnotation ->
    do (translatePatternOrFunction env fa.pattern) <| \patternOrFunction ->
    let
        ( namePattern, params ) =
            case patternOrFunction of
                POF_Pattern p ->
                    ( p, [] )

                POF_Function n pas ->
                    ( CA.PatternAny (tp env.ro fa.pos) n, pas )

        nonRootValues1 =
            if isRoot then
                env.nonRootValues

            else
                Set.union (CA.patternNames namePattern) env.nonRootValues

        localEnv =
            { env | nonRootValues = List.foldl insertParamNames nonRootValues1 params }
    in
    do (translateStatementBlock localEnv fa.body) <| \body ->
    { pattern = namePattern
    , mutable = fa.mutable
    , maybeAnnotation = maybeAnnotation
    , body = List.foldr (wrapLambda env.ro fa.pos) body params
    }
        |> Ok


validateFaDefinition : FA.ValueDef -> Res ()
validateFaDefinition fa =
    let
        maybeName =
            case fa.pattern of
                FA.PatternAny _ _ n ->
                    Just n

                _ ->
                    Nothing
    in
    case fa.maybeAnnotation of
        Nothing ->
            Ok ()

        Just annotation ->
            if fa.mutable /= annotation.mutable then
                errorTodo "annotation mutability doesn't match definition mutability"

            else
                case maybeName of
                    Nothing ->
                        Ok ()

                    Just name ->
                        if annotation.name /= name then
                            errorTodo "annotation name doesn't match definition name"

                        else
                            Ok ()


translateMaybeAnnotation : ReadOnly -> FA.ValueDef -> Res (Maybe CA.Type)
translateMaybeAnnotation ro fa =
    case fa.maybeAnnotation of
        Nothing ->
            Ok Nothing

        Just annotation ->
            translateType ro annotation.ty
                |> Result.map Just



----
--- Pattern-y stuff
--


translateParameter : Env -> FA.Pattern -> Res CA.Parameter
translateParameter env faParam =
    case faParam of
        FA.PatternAny pos True name ->
            Ok <| CA.ParameterMutable (tp env.ro pos) name

        _ ->
            do (translatePattern env faParam) <| \caPattern ->
            Ok <| CA.ParameterPattern caPattern



----
--- Pattern
--


translatePattern : Env -> FA.Pattern -> Res CA.Pattern
translatePattern env faPattern =
    do (translatePatternOrFunction env faPattern) <| \either ->
    case either of
        POF_Pattern caPattern ->
            Ok caPattern

        POF_Function fn params ->
            errorCantDeclareAFunctionHere env fn params faPattern


type POF
    = POF_Pattern CA.Pattern
    | POF_Function String (List CA.Parameter)


translatePatternOrFunction : Env -> FA.Pattern -> Res POF
translatePatternOrFunction env fa =
    case fa of
        FA.PatternAny pos True s ->
            errorTodo "can't mutable here"

        FA.PatternAny pos False s ->
            translatePatternOrFunction env (FA.PatternApplication pos s [])

        FA.PatternLiteral pos l ->
            CA.PatternLiteral (tp env.ro pos) l
                |> POF_Pattern
                |> Ok

        FA.PatternApplication pos rawName faArgs ->
            do (stringToStructuredName { env | maybeUpdateTarget = Nothing } pos rawName) <| \sname ->
            case sname of
                StructuredName_TypeOrCons { name, mod } ->
                    do (Lib.list_mapRes (translatePattern env) faArgs) <| \caArgs ->
                    CA.PatternConstructor (tp env.ro pos) (resolveValueName env.ro False mod name) caArgs
                        |> POF_Pattern
                        |> Ok

                StructuredName_Value { name, mod, attrPath } ->
                    if attrPath /= [] then
                        errorTodo "can't use attribute access inside a pattern"

                    else
                        case mod of
                            AlreadyEmbedded ->
                                errorTodo "can't use attribute shorthands inside a pattern"

                            ResolvedTo _ ->
                                errorTodo "It looks like you are trying to reference some module value, but I need just a new variable name"

                            NotSpecified ->
                                -- it's a function or variable!
                                if faArgs == [] then
                                    name
                                        |> CA.PatternAny (tp env.ro pos)
                                        |> POF_Pattern
                                        |> Ok

                                else
                                    do (Lib.list_mapRes (translateParameter env) faArgs) <| \caParams ->
                                    POF_Function name caParams
                                        |> Ok

        FA.PatternList pos fas ->
            let
                fold pattern last =
                    -- TODO pos is probably inaccurate
                    CA.PatternConstructor (tp env.ro pos) Core.listCons.name [ pattern, last ]
            in
            fas
                |> Lib.list_mapRes (translatePattern env)
                |> Result.map (List.foldr fold (CA.PatternConstructor (tp env.ro pos) Core.listNil.name []) >> POF_Pattern)

        FA.PatternRecord pos recordArgs ->
            if recordArgs.extends /= Nothing then
                errorTodo "can't use `with` inside patterns"

            else
                let
                    fold ( name, maybePattern ) dict =
                        if Dict.member name dict then
                            errorTodo <| "duplicate attribute name in pattern: " ++ name

                        else
                            case maybePattern of
                                Nothing ->
                                    Dict.insert name (CA.PatternAny (tp env.ro pos) name) dict |> Ok

                                Just faPattern ->
                                    faPattern
                                        |> translatePattern env
                                        |> Result.map (\caPattern -> Dict.insert name caPattern dict)
                in
                Lib.list_foldlRes fold recordArgs.attrs Dict.empty
                    |> Result.map (CA.PatternRecord (tp env.ro pos) >> POF_Pattern)

        FA.PatternCons pos pas ->
            do (Lib.list_mapRes (translatePattern env) pas) <| \caPas ->
            case List.reverse caPas of
                last :: rest ->
                    List.foldl
                        (\item list -> CA.PatternConstructor (tp env.ro pos) Core.listCons.name [ item, list ])
                        last
                        rest
                        |> POF_Pattern
                        |> Ok

                [] ->
                    errorTodo "should not happen: empty cons pattern"

        FA.PatternTuple pos fas ->
            case fas of
                [ fa1, fa2 ] ->
                    { extends = Nothing
                    , attrs =
                        [ ( "first", Just fa1 )
                        , ( "second", Just fa2 )
                        ]
                    }
                        |> FA.PatternRecord pos
                        |> translatePatternOrFunction env

                [ fa1, fa2, fa3 ] ->
                    { extends = Nothing
                    , attrs =
                        [ ( "first", Just fa1 )
                        , ( "second", Just fa2 )
                        , ( "third", Just fa3 )
                        ]
                    }
                        |> FA.PatternRecord pos
                        |> translatePatternOrFunction env

                _ ->
                    errorTodo "tuples can be only of size 2 or 3"



----
--- Statement
--


insertDefinedNames : Env -> FA.Statement -> Set String -> Set String
insertDefinedNames env stat names =
    case stat of
        FA.Definition fa ->
            -- TODO is there a clean way to avoid translating the patterns twice?
            case translatePatternOrFunction env fa.pattern of
                Err _ ->
                    names

                Ok (POF_Pattern caPattern) ->
                    Set.union names (CA.patternNames caPattern)

                Ok (POF_Function fnName fnArgs) ->
                    Set.insert fnName names

        _ ->
            names


translateStatementBlock : Env -> List FA.Statement -> Res (List CA.Statement)
translateStatementBlock env stats =
    let
        localEnv =
            { env | nonRootValues = List.foldl (insertDefinedNames env) env.nonRootValues stats }
    in
    Lib.list_mapRes (translateStatement localEnv) stats


translateStatement : Env -> FA.Statement -> Res CA.Statement
translateStatement env faStat =
    case faStat of
        FA.Evaluation faExpr ->
            -- TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            -- Debug evaluations should be optimized away in production build
            faExpr
                |> translateExpression env
                |> Result.map CA.Evaluation

        FA.Definition fa ->
            fa
                |> translateDefinition False env
                |> Result.map CA.Definition

        FA.TypeAlias fa ->
            errorTodo "Aliases can be declared only in the root scope"

        FA.UnionDef fa ->
            errorTodo "Types can be declared only in the root scope"



----
--- Expression
--


translateExpression : Env -> FA.Expression -> Res CA.Expression
translateExpression env faExpr =
    case faExpr of
        FA.Literal pos v ->
            Ok <| CA.Literal (tp env.ro pos) v

        FA.Variable pos { isBinop } faName ->
            if isBinop then
                { isRoot = True
                , name = faName
                , attrPath = []
                }
                    |> CA.Variable (tp env.ro pos)
                    |> Ok

            else
                do (stringToStructuredName env pos faName) <| \sname ->
                let
                    ( name, mod, attrPath ) =
                        case sname of
                            StructuredName_Value a ->
                                ( a.name, a.mod, a.attrPath )

                            StructuredName_TypeOrCons a ->
                                ( a.name, a.mod, [] )

                    declaredInsideFunction =
                        Set.member name env.nonRootValues
                in
                { isRoot = not declaredInsideFunction
                , name = resolveValueName env.ro declaredInsideFunction mod name
                , attrPath = attrPath
                }
                    |> CA.Variable (tp env.ro pos)
                    |> Ok

        FA.Lambda pos faParams faBody ->
            do (Lib.list_mapRes (translateParameter env) faParams) <| \caParams ->
            let
                localEnv =
                    { env | nonRootValues = List.foldl insertParamNames env.nonRootValues caParams }
            in
            case caParams of
                [] ->
                    Debug.todo "TODO should not happen but should be fixable?"

                caHead :: caTail ->
                    do (translateStatementBlock localEnv faBody) <| \caBody ->
                    CA.Lambda (tp env.ro pos) caHead (List.foldr (wrapLambda env.ro pos) caBody caTail)
                        |> Ok

        FA.FunctionCall pos reference arguments ->
            -- ref arg1 arg2 arg3...
            let
                fold : CA.Argument -> CA.Expression -> CA.Expression
                fold argument refAccum =
                    CA.Call (tp env.ro pos) refAccum argument
            in
            Result.map2
                (List.foldl fold)
                (translateExpression env reference)
                (Lib.list_mapRes (translateArgument env) arguments)

        FA.If pos { condition, true, false } ->
            Result.map3
                (\c t f ->
                    CA.If (tp env.ro pos)
                        { condition = [ CA.Evaluation c ]
                        , true = t
                        , false = f
                        }
                )
                (translateExpression env condition)
                (translateStatementBlock env true)
                (translateStatementBlock env false)

        FA.Binop pos group sepList ->
            translateBinops env pos group sepList

        FA.Mutable pos name ->
            errorTodo "mutable values can be used only as arguments for function or mutation operators"

        FA.Record pos faArgs ->
            do (makeUpdateTarget env faArgs.extends) <| \caUpdateTarget ->
            do (translateAttrsRec { env | maybeUpdateTarget = caUpdateTarget.maybeName } faArgs.attrs Dict.empty) <| \caAttrs ->
            caAttrs
                |> CA.Record (tp env.ro pos) caUpdateTarget.maybeName
                |> caUpdateTarget.wrapper
                |> Ok

        FA.List pos faItems ->
            let
                caPos =
                    tp env.ro pos

                cons item list =
                    CA.Call caPos
                        (CA.Call caPos
                            Core.cons
                            (CA.ArgumentExpression item)
                        )
                        (CA.ArgumentExpression list)
            in
            faItems
                -- TODO this is more List.reverse than necessary
                |> Lib.list_mapRes (translateExpression env)
                |> Result.map (List.foldr cons Core.nil)

        FA.Try pos fa ->
            let
                translatePatternAndStatements ( faPattern, faStatements ) =
                    do (translatePattern env faPattern) <| \caPattern ->
                    do (translateStatementBlock { env | nonRootValues = Set.union (CA.patternNames caPattern) env.nonRootValues } faStatements) <| \block ->
                    Ok ( caPattern, block )
            in
            Result.map3
                (\caValue caPatternsAndStatements caElse ->
                    CA.Try (tp env.ro pos) caValue (caPatternsAndStatements ++ caElse)
                )
                (translateExpression env fa.value)
                (Lib.list_mapRes translatePatternAndStatements fa.patterns)
                (case fa.maybeElse of
                    Nothing ->
                        Ok []

                    Just faBlock ->
                        translateStatementBlock env faBlock
                            |> Result.map (\caBlock -> [ ( CA.PatternDiscard (tp env.ro pos), caBlock ) ])
                )

        _ ->
            errorTodo <| "FA expression type not supported for now:" ++ Debug.toString faExpr


makeUpdateTarget : Env -> Maybe FA.Expression -> Res { maybeName : Maybe CA.VariableArgs, wrapper : CA.Expression -> CA.Expression }
makeUpdateTarget env maybeUpdateTarget =
    case Maybe.map (translateExpression { env | maybeUpdateTarget = Nothing }) maybeUpdateTarget of
        Nothing ->
            Ok { maybeName = Nothing, wrapper = identity }

        Just (Err e) ->
            Err e

        Just (Ok (CA.Variable _ args)) ->
            -- TODO test for lowercase name?
            Ok { maybeName = Just args, wrapper = identity }

        Just (Ok expr) ->
            errorTodo "NI { (expr) with ...} not yet implemented =("


translateAttrsRec :
    Env
    -> List ( String, Maybe FA.Expression )
    -> Dict String CA.Expression
    -> Res (Dict String CA.Expression)
translateAttrsRec env faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            Ok caAttrsAccum

        ( attrName, maybeAttrExpression ) :: faTail ->
            let
                exprRes =
                    case maybeAttrExpression of
                        Just faExpr ->
                            translateExpression env faExpr

                        Nothing ->
                            let
                                declaredInsideFunction =
                                    Set.member attrName env.nonRootValues
                            in
                            { name = resolveValueName env.ro declaredInsideFunction NotSpecified attrName
                            , isRoot = not declaredInsideFunction
                            , attrPath = []
                            }
                                |> CA.Variable todoPos
                                |> Ok
            in
            do exprRes <| \expr ->
            translateAttrsRec env faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument : Env -> FA.Expression -> Res CA.Argument
translateArgument env faExpr =
    case faExpr of
        FA.Mutable pos faName ->
            do (stringToStructuredName { env | maybeUpdateTarget = Nothing } pos faName) <| \sname ->
            case sname of
                StructuredName_TypeOrCons _ ->
                    errorTodo "constructors can't be mutable?"

                StructuredName_Value { name, mod, attrPath } ->
                    if mod == NotSpecified && Set.member name env.nonRootValues then
                        { isRoot = False
                        , name = name
                        , attrPath = attrPath
                        }
                            |> CA.ArgumentMutable
                            |> Ok

                    else
                        { sname = sname
                        , env = { env | maybeUpdateTarget = Nothing }
                        , rawName = faName
                        }
                            |> Debug.toString
                            |> (++) "only values declared inside a function scope can be mutated!"
                            |> errorTodo

        _ ->
            faExpr
                |> translateExpression env
                |> Result.map CA.ArgumentExpression


translateBinops : Env -> FA.Pos -> Binop.Precedence -> SepList Binop FA.Expression -> Res CA.Expression
translateBinops env pos group ( firstItem, firstTail ) =
    case firstTail of
        [] ->
            translateExpression env firstItem

        ( firstSep, secondItem ) :: [] ->
            case group of
                Binop.Tuple ->
                    Result.map2
                        (\first second ->
                            Dict.empty
                                |> Dict.insert "first" first
                                |> Dict.insert "second" second
                                |> CA.Record (tp env.ro pos) Nothing
                        )
                        (translateExpression env firstItem)
                        (translateExpression env secondItem)

                _ ->
                    translateSimpleBinop env pos firstItem firstSep secondItem

        ( firstSep, secondItem ) :: ( secondSep, thirdItem ) :: thirdTail ->
            let
                -- Use "as"? Yay undocumented syntax...
                secondTail =
                    ( secondSep, thirdItem ) :: thirdTail
            in
            case group of
                Binop.Comparison ->
                    if notAllSeparators (sameDirectionAs firstSep) secondTail then
                        -- TODO actually list the seps
                        errorTodo "can't mix comparison ops with different direction"

                    else
                        -- TODO expand `a < b < c` to `a < b && b < c` without calculating b twice
                        errorTodo "NI compops expansion"

                Binop.Logical ->
                    if notAllSeparators ((==) firstSep) secondTail then
                        errorTodo "Mixing `and` and `or` is ambiguous. Use parens!"

                    else
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                Binop.Tuple ->
                    if thirdTail /= [] then
                        errorTodo "Tuples can't have more than 3 items, use a record instead."

                    else
                        Result.map3
                            (\first second third ->
                                Dict.empty
                                    |> Dict.insert "first" first
                                    |> Dict.insert "second" second
                                    |> Dict.insert "third" third
                                    |> CA.Record (tp env.ro pos) Nothing
                            )
                            (translateExpression env firstItem)
                            (translateExpression env secondItem)
                            (translateExpression env thirdItem)

                Binop.Pipe ->
                    if notAllSeparators ((==) firstSep) secondTail then
                        errorTodo "Mixing pipes is ambigous. Use parens."

                    else if firstSep.associativity == Binop.Right then
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                    else
                        translateBinopSepList_leftAssociative env pos firstItem firstTail

                Binop.Mutop ->
                    errorTodo "mutops can't be chained"

                _ ->
                    translateBinopSepList_rightAssociative env pos firstItem firstTail


notAllSeparators : (sep -> Bool) -> List ( sep, item ) -> Bool
notAllSeparators f ls =
    case ls of
        [] ->
            False

        ( sep, item ) :: tail ->
            if f sep then
                notAllSeparators f tail

            else
                True


sameDirectionAs : Binop -> Binop -> Bool
sameDirectionAs a b =
    if a.symbol == b.symbol then
        True

    else
        case a.symbol of
            ">" ->
                b.symbol == ">="

            ">=" ->
                b.symbol == ">"

            "<" ->
                b.symbol == "<="

            "<=" ->
                b.symbol == "<"

            _ ->
                False


translateBinopSepList_rightAssociative : Env -> FA.Pos -> FA.Expression -> List ( Binop, FA.Expression ) -> Res CA.Expression
translateBinopSepList_rightAssociative env pos left opsAndRight =
    do (translateExpression env left) <| \caLeft ->
    case opsAndRight of
        [] ->
            Ok caLeft

        ( op, right ) :: tail ->
            do (translateBinopSepList_rightAssociative env pos right tail) <| \caRight ->
            makeBinop (tp env.ro pos) (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)
                |> Ok


translateBinopSepList_leftAssociative : Env -> FA.Pos -> FA.Expression -> List ( Binop, FA.Expression ) -> Res CA.Expression
translateBinopSepList_leftAssociative env pos leftAccum opsAndRight =
    do (translateExpression env leftAccum) <| \caLeftAccum ->
    translateBinopSepListRec env pos caLeftAccum opsAndRight


translateBinopSepListRec : Env -> FA.Pos -> CA.Expression -> List ( Binop, FA.Expression ) -> Res CA.Expression
translateBinopSepListRec env pos leftAccum opsAndRight =
    case opsAndRight of
        [] ->
            Ok leftAccum

        ( op, faRight ) :: tail ->
            do (translateArgument env faRight) <| \caRight ->
            translateBinopSepListRec env pos (makeBinop (tp env.ro pos) (CA.ArgumentExpression leftAccum) op caRight) tail


{-| Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

-}
makeBinop : CA.Pos -> CA.Argument -> Binop -> CA.Argument -> CA.Expression
makeBinop caPos left op right =
    case ( left, op.symbol, right ) of
        -- TODO don't hardcode the strings, use instead those defined in Prelude
        ( _, ">>", CA.ArgumentExpression rightExpr ) ->
            CA.Call caPos rightExpr left

        ( CA.ArgumentExpression leftExpr, "<<", _ ) ->
            CA.Call caPos leftExpr right

        _ ->
            CA.Call caPos
                (CA.Call caPos
                    (CA.Variable caPos
                        { isRoot = True
                        , name = op.symbol
                        , attrPath = []
                        }
                    )
                    right
                )
                left


translateSimpleBinop : Env -> FA.Pos -> FA.Expression -> Binop -> FA.Expression -> Res CA.Expression
translateSimpleBinop env pos left op right =
    Result.map2 (\l r -> makeBinop (tp env.ro pos) l op r)
        (translateArgument env left)
        (translateArgument env right)



----
--- Type
--


addAttributes : ReadOnly -> List ( String, Maybe FA.Type ) -> Dict String CA.Type -> Res CA.Type
addAttributes ro faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            CA.TypeRecord todoPos Nothing caAttrsAccum
                |> Ok

        ( name, maybeFaType ) :: faTail ->
            let
                faType =
                    Maybe.withDefault (FA.TypeName {- TODO -} ( -1, -1 ) name) maybeFaType
            in
            do (translateType ro faType) <| \caType ->
            addAttributes ro faTail (Dict.insert name caType caAttrsAccum)


startsWithUpperChar : String -> Bool
startsWithUpperChar s =
    case String.uncons s of
        Nothing ->
            False

        Just ( head, tail ) ->
            Char.isUpper head


translateType : ReadOnly -> FA.Type -> Res CA.Type
translateType ro faType =
    case faType of
        FA.TypeName p name ->
            translateType ro <| FA.TypePolymorphic p name []

        FA.TypePolymorphic pos rawName args ->
            do (stringToStructuredName (initEnv ro) pos rawName) <| \sname ->
            case sname of
                StructuredName_Value { name, mod, attrPath } ->
                    if args /= [] then
                        -- TODO is this the correct error?
                        errorTodo "rank 2 types are not supported"

                    else if mod /= NotSpecified then
                        errorTodo "this is not a valid name for a type variable"

                    else if attrPath /= [] then
                        errorTodo "no attribute accessors on types"

                    else
                        name
                            |> CA.TypeVariable todoPos
                            |> Ok

                StructuredName_TypeOrCons { name, mod } ->
                    do (Lib.list_mapRes (translateType ro) args) <| \caArgs ->
                    caArgs
                        |> CA.TypeConstant (tp ro pos) (resolveTypeName ro False mod name)
                        |> Ok

        FA.TypeFunction pos fa_from fromIsMut fa_to ->
            Result.map2
                (\ca_from ca_to -> CA.TypeFunction (tp ro pos) ca_from fromIsMut ca_to)
                (translateType ro fa_from)
                (translateType ro fa_to)

        FA.TypeTuple pos types ->
            case types of
                [ faFirst, faSecond ] ->
                    Result.map2
                        (\caFirst caSecond ->
                            Dict.empty
                                |> Dict.insert "first" caFirst
                                |> Dict.insert "second" caSecond
                                |> CA.TypeRecord (tp ro pos) Nothing
                        )
                        (translateType ro faFirst)
                        (translateType ro faSecond)

                [ faFirst, faSecond, faThird ] ->
                    Result.map3
                        (\caFirst caSecond caThird ->
                            Dict.empty
                                |> Dict.insert "first" caFirst
                                |> Dict.insert "second" caSecond
                                |> Dict.insert "third" caThird
                                |> CA.TypeRecord (tp ro pos) Nothing
                        )
                        (translateType ro faFirst)
                        (translateType ro faSecond)
                        (translateType ro faThird)

                _ ->
                    errorTodo "Tuples can only have size 2 or 3. Use a record."

        FA.TypeRecord p recordArgs ->
            if recordArgs.extends /= Nothing then
                errorExperimentingWithNoExtensibleTypes ro p

            else
                addAttributes ro recordArgs.attrs Dict.empty


errorExperimentingWithNoExtensibleTypes : ReadOnly -> FA.Pos -> Res a
errorExperimentingWithNoExtensibleTypes ro ( start, end ) =
    Error.makeRes ro.currentModule
        [ Error.showLines ro.code 2 start
        , Error.text "For now extensible types are disabled, I want to see if it's good to do without them"
        ]



----
--- Helpers
--


wrapLambda : ReadOnly -> FA.Pos -> CA.Parameter -> List CA.Statement -> List CA.Statement
wrapLambda ro faWholePos param bodyAccum =
    let
        end =
            Tuple.second faWholePos

        paramPos =
            case param of
                CA.ParameterMutable pos name ->
                    pos

                CA.ParameterPattern pa ->
                    CA.patternPos pa

        lambdaPos =
            { paramPos | e = end }
    in
    [ bodyAccum
        |> CA.Lambda lambdaPos param
        |> CA.Evaluation
    ]


firstErrorRec : List o -> List (Result e o) -> Result e (List o)
firstErrorRec os rs =
    case rs of
        [] ->
            Ok os

        (Err e) :: _ ->
            Err e

        (Ok o) :: rt ->
            firstErrorRec (o :: os) rt


maybeResultToResultMaybe : Maybe (Result e o) -> Result e (Maybe o)
maybeResultToResultMaybe maybeResult =
    case maybeResult of
        Nothing ->
            Ok Nothing

        Just (Ok something) ->
            Ok (Just something)

        Just (Err e) ->
            -- Unfortunately Elm doesn't realize that the two `Err e` have the same type
            -- Is it a limit of how pattern matching is implemented?
            Err e



----
--- Errors
--


errorRecordUpdateShorthandOutsideRecordUpdate : FA.Pos -> String -> Env -> Res a
errorRecordUpdateShorthandOutsideRecordUpdate ( start, end ) rawString env =
    Error.makeRes
        env.ro.currentModule
        [ Error.showLines env.ro.code 2 start
        , Error.text <| Error.inlineCode rawString ++ " looks like a record update shorthand, but we are not inside a record update!"
        ]


errorCantDeclareAFunctionHere : Env -> String -> List CA.Parameter -> FA.Pattern -> Res a
errorCantDeclareAFunctionHere env name params originalPattern =
    Error.makeRes
        env.ro.currentModule
        [ Error.showLines env.ro.code 2 (Tuple.first <| FA.patternPos originalPattern)
        , Error.text "it seems like there is a function declaration inside a pattern?"
        ]


errorRootDefinitionsCantBeMutable : Env -> CA.LocalValueDef -> Res a
errorRootDefinitionsCantBeMutable env def =
    Error.makeRes
        env.ro.currentModule
        [ Error.showLines env.ro.code 2 (CA.patternPos def.pattern).s
        , Error.text "mutable values can be declared only inside functions."
        ]
