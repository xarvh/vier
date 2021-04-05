module Compiler.CanonicalToJs exposing (..)

import Compiler.CoreModule
import Dict exposing (Dict)
import Lib
import RefHierarchy
import Set exposing (Set)
import Types.CanonicalAst as CA
import Types.JavascriptAst as JA
import Types.Literal


nativeNonOps : Dict String JA.Name
nativeNonOps =
    nativeBinops
        |> Dict.map (always .fnName)
        |> Dict.insert Compiler.CoreModule.trueValue "true"
        |> Dict.insert Compiler.CoreModule.falseValue "false"
        |> Dict.insert Compiler.CoreModule.noneValue "null"
        |> Dict.insert "SPCore/Debug.log" "sp_log"
        |> Dict.insert "SPCore/Debug.todo" "sp_todo"
        |> Dict.insert "SPCore/Text.fromInt" "text_fromInt"
        |> Dict.insert "/" "sp_divide"


nativeBinops : Dict String { jsSymb : JA.Name, mutates : Bool, fnName : String }
nativeBinops =
    Dict.empty
        |> Dict.insert "+" { jsSymb = "+", mutates = False, fnName = "add" }
        |> Dict.insert "*" { jsSymb = "*", mutates = False, fnName = "mul" }
        |> Dict.insert "-" { jsSymb = "-", mutates = False, fnName = "sub" }
        |> Dict.insert ":=" { jsSymb = "=", mutates = True, fnName = "mutass" }
        |> Dict.insert "+=" { jsSymb = "+=", mutates = True, fnName = "mutadd" }
        |> Dict.insert ".." { jsSymb = "+", mutates = False, fnName = "strcon" }
        |> Dict.insert ">" { jsSymb = ">", mutates = False, fnName = "greaterThan" }
        |> Dict.insert "<" { jsSymb = "<", mutates = False, fnName = "lesserThan" }


none =
    translatePath Compiler.CoreModule.noneValue


nativeBinopToFunction : String -> { jsSymb : JA.Name, mutates : Bool, fnName : String } -> List JA.Statement -> List JA.Statement
nativeBinopToFunction spName { jsSymb, mutates, fnName } acc =
    let
        d =
            CA.posDummy
    in
    ([ CA.Evaluation
        (CA.Lambda d
            (CA.PatternAny d "a")
            [ CA.Evaluation
                (CA.Lambda d
                    (CA.PatternAny d "b")
                    [ CA.Evaluation
                        (CA.Call d
                            (CA.Call d
                                (CA.Variable d { attrPath = [], isRoot = True, name = spName })
                                (CA.ArgumentExpression
                                    (CA.Variable d { attrPath = [], isRoot = False, name = "a" })
                                )
                            )
                            ((if mutates then
                                CA.ArgumentMutable

                              else
                                CA.ArgumentExpression << CA.Variable d
                             )
                                { attrPath = [], isRoot = False, name = "b" }
                            )
                        )
                    ]
                )
            ]
        )
     ]
        |> translateBodyToExpr Set.empty
        |> JA.Define fnName
    )
        :: acc


nativeBinopsAsFns =
    Dict.foldl nativeBinopToFunction [] nativeBinops



{---
--- Mutation
--

  CA.Definition: `x @= 0`

     => `const x = { obj : { m : 0 }, attr : 'm' };`



  CA.Variable: `x`

     => `$clone$(x.obj[x.attr])`

     (when we have a non-variable type we can optimize this)



  CA.Call (mutation primitive): `@x += 1`

      CA.ArgumentMutable, fn is primitive: `@x := 0`

        => `x.obj[x.attr] := 0`


      CA.ArgumentMutable, fn is NOT primitive
        if attrPath == []: `doStuff @x`

          => `doStuff(x)`


        else: `doStuff @x.a.b.c`

          => `doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })



-}


{-| This right now just contains the names of mutable vars
-}
type alias Env =
    Set JA.Name


wrapMutable : Bool -> JA.Expr -> JA.Expr
wrapMutable mutable expr =
    if mutable then
        Dict.empty
            |> Dict.insert "attr" (JA.Literal <| quoteAndEscape "$")
            |> Dict.insert "obj" (JA.Record <| Dict.singleton "$" expr)
            |> JA.Record

    else
        expr


unwrapMutable : JA.Name -> JA.Expr
unwrapMutable x =
    let
        ja_x =
            JA.Var x
    in
    -- $x.obj[$x.attr]
    JA.AccessWithBrackets (JA.AccessWithDot "attr" ja_x) (JA.AccessWithDot "obj" ja_x)


nativeDefinitions : String
nativeDefinitions =
    """
const sp_clone = (src) => {
 if (Array.isArray(src))
   return src.map(clone);

 if (typeof(src) === 'object') {
   const dest = {};
   for (let k in src) { dest[k] = sp_clone(src[k]); }
   return dest;
 }

 return src;
}


const sp_divide = (right) => (left) => {
  if (right === 0) return 0;
  return left / right;
}


const sp_todo = (message) => {
  throw new Error("TODO: " + message);
}

const sp_log = (message) => (thing) => {
  console.log(message, thing);
  return thing;
}


const text_fromInt = (n) => '' + n;
    """


clone : JA.Expr -> JA.Expr
clone expr =
    JA.Call (JA.Var "sp_clone") [ expr ]



{---
--- Name translations
--


   We need to give JS names to
     - the variables declared within the SP code
     - the variables we generate as part of the SP->JS translation

   and we need to ensure that these names do not clash with each other
   and do not clash with JS reserved keywords.

   So when we need a new naming schema, we follow this:

   * no $ anywhere
     => it's a native JS value

   * starts and ends with a single $
     => $clone$

   * starts with a single $
     * followed by a letter
       => it corresponds to an SP path or local value
     * followed by a number
       => it's a constructor argument

     * starts with double $$
       => $$try
       => main name of an unpacked pattern

-}


translatePath : String -> JA.Name
translatePath s =
    case Dict.get s nativeNonOps of
        Just nv ->
            nv

        Nothing ->
            s
                |> String.replace "." "$"
                |> String.replace "/" "$"
                |> (++) "$"


constructorArgumentName : Int -> JA.Name
constructorArgumentName i =
    "$" ++ String.fromInt i


tryName : JA.Name
tryName =
    "$$try"


pickMainName : CA.Pattern -> Maybe JA.Name
pickMainName pattern =
    Maybe.map (String.replace "/" "$" >> String.replace "." "$") <|
        case pattern of
            CA.PatternAny _ name ->
                Just <| "$" ++ name

            _ ->
                pattern
                    |> CA.patternNames
                    |> Set.toList
                    |> List.head
                    |> Maybe.map ((++) "$$")



----
--- Translation
--


getValueDefName : CA.ValueDef -> String
getValueDefName def =
    def.pattern
        |> CA.patternNames
        |> Set.toList
        |> List.head
        |> Maybe.withDefault "BLARGH"


getValueRefs : CA.ValueDef -> Set String
getValueRefs def =
    let
        fn fold ( ext, set ) =
            case fold of
                CA.FoldExpr (CA.Variable _ args) ->
                    ( ext
                    , if args.isRoot then
                        Set.insert args.name set

                      else
                        set
                    )

                _ ->
                    ( ext, set )
    in
    ( def, Set.empty )
        |> CA.extensionFold_valueDef fn
        |> Tuple.second


translateAll : CA.AllDefs -> List JA.Statement
translateAll ca =
    let
        ( aliases, unions, values ) =
            CA.split ca

        cons =
            unions
                |> Dict.values
                |> List.sortBy .name
                |> List.concatMap translateUnion

        isFunctionBlock : CA.ValueDef -> Bool
        isFunctionBlock def =
            case def.body of
                (CA.Evaluation (CA.Lambda _ _ _)) :: [] ->
                    True

                _ ->
                    False

        ( fns, nonFns ) =
            values
                |> Dict.values
                |> List.partition isFunctionBlock

        reorderedNonFuns =
            RefHierarchy.reorder getValueDefName getValueRefs nonFns

        vals =
            (fns ++ reorderedNonFuns)
                |> List.concatMap (translateValueDef Set.empty >> Tuple.first)
    in
    nativeBinopsAsFns ++ cons ++ vals


translateValueDef : Env -> CA.ValueDef -> ( List JA.Statement, Env )
translateValueDef env caDef =
    if caDef.body == [] then
        ( [], env )

    else
        case pickMainName caDef.pattern of
            Nothing ->
                ( [ JA.Eval (translateBodyToExpr env caDef.body) ]
                , env
                )

            Just mainName ->
                ( (caDef.body
                    |> translateBodyToExpr env
                    |> wrapMutable caDef.mutable
                    |> JA.Define mainName
                  )
                    :: patternDefinitions mainName caDef.pattern
                , if caDef.mutable then
                    Set.insert mainName env

                  else
                    env
                )


translateStatement : Env -> CA.Statement -> ( List JA.Statement, Env )
translateStatement env s =
    case s of
        CA.Definition def ->
            translateValueDef env def

        CA.Evaluation expr ->
            ( [ JA.Eval (translateExpr env expr) ]
            , env
            )


translateVar : Env -> CA.VariableArgs -> JA.Expr
translateVar env { name, attrPath } =
    let
        jname =
            translatePath name
    in
    -- right now `env` just tells us whether a var is mutable or not
    if Set.member jname env then
        jname
            |> unwrapMutable
            |> accessAttrs attrPath
            |> clone

    else
        accessAttrs attrPath (JA.Var jname)


translateLiteral : Types.Literal.Value -> String
translateLiteral value =
    case value of
        Types.Literal.Text t ->
            quoteAndEscape t

        Types.Literal.Number n ->
            n

        Types.Literal.Char c ->
            -- TODO how will this interact with other function calls?
            "\"" ++ String.fromChar c ++ "\""


translateExpr : Env -> CA.Expression -> JA.Expr
translateExpr env expression =
    case expression of
        CA.Literal _ lit ->
            lit
                |> translateLiteral
                |> JA.Literal

        CA.Variable _ ar ->
            translateVar env ar

        CA.Lambda _ parameter body ->
            let
                ( args, extraJaStatements ) =
                    case pickMainName parameter of
                        Nothing ->
                            ( [], [] )

                        Just mainName ->
                            ( [ mainName ]
                            , patternDefinitions mainName parameter
                            )
            in
            case translateBodyToEither env extraJaStatements body of
                Lib.Left expr ->
                    JA.SimpleLambda args expr

                Lib.Right block ->
                    JA.BlockLambda args block

        CA.Record _ extends attrs ->
            let
                obj =
                    attrs
                        |> Dict.map (\k -> translateExpr env)
                        |> JA.Record
            in
            case extends of
                Nothing ->
                    obj

                Just extend ->
                    JA.Call (JA.Var "Object.assign")
                        [ JA.Record Dict.empty
                        , translateVar env extend
                        , obj
                        ]

        CA.Call _ ref arg ->
            case maybeNativeBinop env ref arg of
                Just jaExpr ->
                    jaExpr

                Nothing ->
                    JA.Call
                        (translateExpr env ref)
                        [ translateArg { nativeBinop = False } env arg ]

        CA.If _ ar ->
            JA.Conditional
                (translateBodyToExpr env ar.condition)
                (translateBodyToExpr env ar.true)
                (translateBodyToExpr env ar.false)

        CA.Try pos value tries ->
            {-

               (() => {
                 const $$try = #value;

                 if (#pattern1Conditions) {
                   #pattern1Assignment
                   #pattern1Block
                   return
                 }

                 if (#pattern2Conditions) {
                   #pattern2Assignment
                   #pattern2Block
                   return
                 }
               )


            -}
            let
                head =
                    JA.Define tryName (translateExpr env value)

                init =
                    JA.Var tryName

                testPa ( pattern, block ) =
                    let
                        extraStats =
                            patternDefinitions tryName pattern

                        condition =
                            testPattern pattern init []
                                |> binopChain (JA.Literal "true") "&&"

                        whenConditionMatches =
                            case translateBodyToEither env extraStats block of
                                Lib.Left e ->
                                    extraStats ++ [ JA.Return e ]

                                Lib.Right bl ->
                                    -- TODO two returns is better than zero, but one would be better than two...
                                    bl ++ [ JA.Return (JA.Literal "null") ]
                    in
                    JA.If condition whenConditionMatches

                allStatements =
                    head :: List.map testPa tries
            in
            JA.Call (JA.BlockLambda [] allStatements) []


translateArg : { nativeBinop : Bool } -> Env -> CA.Argument -> JA.Expr
translateArg { nativeBinop } env arg =
    case arg of
        CA.ArgumentExpression e ->
            translateExpr env e

        CA.ArgumentMutable { name, attrPath } ->
            if nativeBinop then
                --SP: @x.a.b += blah
                --JS: x.obj[x.attr].a.b += blah
                name
                    |> translatePath
                    |> unwrapMutable
                    |> accessAttrs attrPath

            else
                case attrPath of
                    [] ->
                        --SP: doStuff @x
                        --JS: doStuff(x)
                        name
                            |> translatePath
                            |> JA.Var

                    head :: tail ->
                        --SP: doStuff @x.a.b.c
                        --JS: doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })
                        name
                            |> translatePath
                            |> unwrapMutable
                            |> accessAttrsButTheLast head tail
                            |> (\( wrappedExpr, lastAttrName ) ->
                                    Dict.empty
                                        |> Dict.insert "obj" wrappedExpr
                                        |> Dict.insert "attr" (JA.Literal <| quoteAndEscape lastAttrName)
                                        |> JA.Record
                               )


accessAttrs : List String -> JA.Expr -> JA.Expr
accessAttrs attrPath e =
    List.foldl JA.AccessWithDot e attrPath


accessAttrsButTheLast : String -> List String -> JA.Expr -> ( JA.Expr, String )
accessAttrsButTheLast attrHead attrTail e =
    let
        fold attr ( expr, last ) =
            ( JA.AccessWithDot last expr
            , attr
            )
    in
    List.foldl fold ( e, attrHead ) attrTail


maybeNativeBinop : Env -> CA.Expression -> CA.Argument -> Maybe JA.Expr
maybeNativeBinop env reference argument =
    case reference of
        CA.Call _ (CA.Variable _ { name }) rightArg ->
            case Dict.get name nativeBinops of
                Nothing ->
                    Nothing

                Just { jsSymb, mutates } ->
                    let
                        cons =
                            if mutates then
                                JA.Mutop jsSymb none

                            else
                                JA.Binop jsSymb
                    in
                    cons
                        (translateArg { nativeBinop = True } env argument)
                        (translateArg { nativeBinop = True } env rightArg)
                        |> Just

        _ ->
            Nothing


binopChain : JA.Expr -> String -> List JA.Expr -> JA.Expr
binopChain default op es =
    case es of
        [] ->
            default

        head :: tail ->
            List.foldl (JA.Binop op) head tail


translateBodyToExpr : Env -> List CA.Statement -> JA.Expr
translateBodyToExpr env caBody =
    case translateBodyToEither env [] caBody of
        Lib.Left e ->
            e

        Lib.Right block ->
            JA.Call (JA.BlockLambda [] block) []


translateBodyToEither : Env -> List JA.Statement -> List CA.Statement -> Lib.Either JA.Expr (List JA.Statement)
translateBodyToEither env extra caBody =
    let
        fold caStat ( faStats, e ) =
            translateStatement e caStat
                |> Tuple.mapFirst (List.foldl (::) faStats)

        ( reversedStats, env1 ) =
            List.foldl fold ( extra, env ) caBody
    in
    case reversedStats of
        [] ->
            JA.Var "null"
                |> Lib.Left

        [ JA.Eval single ] ->
            single
                |> Lib.Left

        oldLast :: rest ->
            (case oldLast of
                JA.Eval expr ->
                    JA.Return expr :: rest

                _ ->
                    JA.Return (JA.Var none) :: oldLast :: rest
            )
                |> List.reverse
                |> Lib.Right



----
---
--


quoteAndEscape : String -> String
quoteAndEscape s =
    -- TODO escape
    "\"" ++ s ++ "\""


accessWithBracketsInt : Int -> JA.Expr -> JA.Expr
accessWithBracketsInt index =
    index
        |> String.fromInt
        |> JA.Var
        |> JA.AccessWithBrackets



----
--- Pattern break down
--


testPattern : CA.Pattern -> JA.Expr -> List JA.Expr -> List JA.Expr
testPattern pattern valueToTest accum =
    case pattern of
        CA.PatternDiscard _ ->
            accum

        CA.PatternAny _ name ->
            accum

        CA.PatternLiteral _ lit ->
            JA.Binop "===" (JA.Literal <| translateLiteral lit) valueToTest :: accum

        CA.PatternConstructor _ path pas ->
            let
                head =
                    JA.Binop "==="
                        (JA.Literal <| quoteAndEscape path)
                        (accessWithBracketsInt 0 valueToTest)

                foldArg argPattern ( index, acc ) =
                    ( index + 1
                    , testPattern argPattern (accessWithBracketsInt index valueToTest) acc
                    )
            in
            List.foldl foldArg ( 1, head :: accum ) pas
                |> Tuple.second

        CA.PatternRecord _ attrs ->
            let
                foldAttr name pa =
                    testPattern pa (JA.AccessWithDot name valueToTest)
            in
            Dict.foldl foldAttr accum attrs


assignPattern : CA.Pattern -> JA.Expr -> List JA.Statement -> List JA.Statement
assignPattern pattern exprAccum accum =
    case pattern of
        CA.PatternDiscard _ ->
            accum

        CA.PatternAny _ name ->
            if name == "_" then
                accum

            else
                JA.Define (translatePath name) exprAccum :: accum

        CA.PatternLiteral _ literal ->
            accum

        CA.PatternConstructor _ path pas ->
            let
                foldEveryArgument ( index, pa ) =
                    assignPattern pa (accessConstructorArg (index + 1) exprAccum)
            in
            List.foldl foldEveryArgument accum (List.indexedMap Tuple.pair pas)

        CA.PatternRecord _ attrs ->
            let
                foldEveryAttr name pa =
                    assignPattern pa (JA.AccessWithDot name exprAccum)
            in
            Dict.foldl foldEveryAttr accum attrs


patternDefinitions : JA.Name -> CA.Pattern -> List JA.Statement
patternDefinitions mainName pattern =
    case pattern of
        CA.PatternAny pos _ ->
            []

        _ ->
            assignPattern pattern (JA.Var mainName) []



----
--- Union Constructors
--


accessConstructorArg : Int -> JA.Expr -> JA.Expr
accessConstructorArg =
    accessWithBracketsInt


translateUnionConstructor : ( String, List CA.Type ) -> JA.Statement
translateUnionConstructor ( consName, consArgs ) =
    -- const ConstructorName = ($1) => ($2) => [ "ConstructorName", $1, $2 ]
    let
        n =
            List.length consArgs

        range =
            List.range 1 n

        storedArgs =
            List.map (constructorArgumentName >> JA.Var) range

        name =
            quoteAndEscape consName

        expr =
            JA.Array (JA.Literal name :: storedArgs)

        lambdas =
            List.foldr (\i -> JA.SimpleLambda [ constructorArgumentName i ]) expr range
    in
    JA.Define (translatePath consName) lambdas


translateUnion : CA.UnionDef -> List JA.Statement
translateUnion def =
    def.constructors
        |> Dict.toList
        |> List.filter (\( name, args ) -> not <| Dict.member name nativeNonOps)
        |> List.map translateUnionConstructor



--
