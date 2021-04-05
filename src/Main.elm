module Main exposing (..)

import Array exposing (Array)
import Browser
import Compiler.ApplyAliases
import Compiler.ApplyAliases_Test
import Compiler.CanonicalToJs
import Compiler.CanonicalToJs_Test
import Compiler.FormattableToCanonicalAst
import Compiler.FormattableToCanonicalAst_Test
import Compiler.JsToString
import Compiler.JsToString_Test
import Compiler.Pipeline
import Compiler.StringToTokens
import Compiler.StringToTokens_Test
import Compiler.TestHelpers
import Compiler.TokensToFormattableAst
import Compiler.TokensToFormattableAst_Test
import Compiler.TypeInference as TI
import Compiler.TypeInference_Test
import Css
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Lib
import Markdown
import MetaFile
import OneOrMore exposing (OneOrMore)
import Parser
import Prelude
import Set exposing (Set)
import Test
import Types.CanonicalAst as CA exposing (Pos)
import Types.Error exposing (Res)
import Types.FormattableAst as FA
import Types.Literal as Literal
import Types.Meta exposing (Meta)
import Types.Token as Token exposing (Token)


runTests =
    False
        || True


initialFiles =
    [ moduleMain
    , moduleMaybe
    , moduleList
    , moduleText
    , languageOverview
    , ( metaFileName, Prelude.metaString )
    ]
        |> List.map (Tuple.mapSecond Compiler.TestHelpers.unindent)


metaFileName =
    "meta"


moduleMain =
    ( "Main"
    , """
      result x y = 3
      """
    )


moduleMaybe =
    ( "SPCore/Maybe"
    , """
      union Maybe a = Nothing, Just a

      andThen : Maybe a -> (a -> Maybe b) -> Maybe b
      andThen ma f =
          try ma as
              Nothing then
                  Nothing
              Just a then
                  f a

      map : (a -> b) -> Maybe a -> Maybe b
      map f m =
        try m as
          Nothing then
            Nothing
          Just v then
            Just (f v)
        """
    )


moduleText =
    ( "SPCore/Text"
    , """
      fromInt : Number -> Text
      fromInt n =
        "<native>"

      join : Text -> List Text -> Text
      join sep listOfText =
        try listOfText as
            SPCore.Nil then
              ""

            SPCore.Cons head tail then
              rec ls acc =
                try ls as
                  SPCore.Nil then
                    acc
                  SPCore.Cons h t then
                    rec t << acc .. sep .. h

              rec tail head
        """
    )


moduleList =
    ( "SPCore/List"
    , """
each : List a -> (a -> b) -> None
each ls f =
    try ls as
        SPCore.Nil then
            None

        SPCore.Cons head tail then
            f head
            each tail f

reverse : List a -> List a
reverse aList =
    rec ls acc =
        try ls as
            SPCore.Nil then
                acc

            SPCore.Cons head tail then
                rec tail (SPCore.Cons head acc)

    rec aList []
        """
    )


languageOverview =
    ( "Language/Overview"
    , """
[#
   SquarePants has no import statements: instead, project-wide imports are
   declared in the `meta` file.
#]


# Declare a constant
numberOne =
    1

# Declare a function
addThreeNumbers x y z =
    x + y + z


# TODO: polymorphism for number types is not yet implemented,
# so I'm cheating and adding these aliases.
alias Int = Number
alias Float = Number
alias Vec2 = Number


# Declarations can have a type annotation
floatSix : Float
floatSix =
    # parens are not needed for calling functions
    addThreeNumbers 1 2 3


# if-then-else always yields a value
fibonacci : Int -> Int
fibonacci n =
    if n < 2 then n else n + fibonacci (n - 1)


# operators can be used prefixed, like functions
# `left - right` becomes `(-) right left`
subtractTwoFrom : Vec2 -> Vec2
subtractTwoFrom n =
    (-) 2 n


# Square brackets for Lists.
# All items must be of the same type
listOfText : [ Text ]
listOfText = [
    , "Gary"
    , "Bikini Bottom"
    , "I'm ready! Promotion!"
    ]


# `>>` and `<<` are just syntactic sugar
# They help using less parens and
# help visualizing how a value is transformed
repeatHello : Int -> Text
repeatHello times =
    listOfText
        >> List.reverse
        >> Text.join ", "
        >> (..) " and append this text at the end"


# When you see `@`, it means "this stuff is mutable"
average : List Int -> Float
average numbers =
    # mutable variables can only be local and can't leave their scope.
    # `average` is still a pure function.
    n @= 0
    sum @= 0

    # anonymous functions start with `fn`
    List.each numbers fn x =
        @n += 1
        @sum += x

    # division by 0 yields 0
    sum / n


[# TODO: implement Random.
# The argument preceding `@>` is mutable
generateTwoRandomNumbers : Int -> Int -> Random.Seed @> Int & Int
generateTwoRandomNumbers min max seed =
    # '&' is used for tuples
    Random.int min max @seed & Random.int min max @seed
#]



# Algebraic Data Types

union LoadingState payload =
    , NotRequested
    , Requested
    , Error Text
    , Available payload

getStatusName : LoadingState payload -> Text
getStatusName loadingState =
    try loadingState as
        NotRequested then "Not needed"
        Requested then "Awaiting server response"
        Error message then "Error: " .. message
        Available _ then "Successfully loaded"

getPayload : LoadingState payload -> Maybe payload
getPayload loadingState =
    try loadingState as Available payload then Just payload else Nothing



# Records

alias Crab = {
    , name : Text
    , money : Float
    }

eugeneKrabs : Crab
eugeneKrabs = {
    , name = "Eugene H. Krabs"
    , money = 2 #TODO 2_345_678.90
    }


# TODO add a record access example


earnMoney : Float -> Crab -> Crab
earnMoney profit crab =
    # `.money` is a shorthand for `crab.money`
    { crab with money = .money + profit }


# to-notation
getAllHouses : (Text -> Maybe house) -> Maybe { rock : house, moai : house, pineapple : house }
getAllHouses getAsset =
    to = Maybe.andThen
    getAsset "rock" >> to fn rock =
    getAsset "moai" >> to fn moai =
    getAsset "pineapple" >> to fn pineapple =
       Just { rock, moai, pineapple }
    """
    )


tests =
    if not runTests then
        Html.text ""

    else
        Test.viewList
            [ Compiler.StringToTokens_Test.tests
            , Compiler.TokensToFormattableAst_Test.tests
            , Compiler.TypeInference_Test.tests

            --             , Compiler.FindUndeclared_Test.tests
            , Compiler.ApplyAliases_Test.tests
            , Compiler.JsToString_Test.tests
            , Compiler.CanonicalToJs_Test.tests
            , Compiler.FormattableToCanonicalAst_Test.tests
            ]


type alias Model =
    { files : Dict String String
    , selectedFile : String
    }


type Msg
    = OnInput String
    | OnSelect String


init : Model
init =
    { files = Dict.fromList initialFiles
    , selectedFile =
        --         "Language/Overview"
        initialFiles
            |> List.head
            |> Maybe.map Tuple.first
            |> Maybe.withDefault ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnInput code ->
            { model | files = Dict.insert model.selectedFile code model.files }

        OnSelect name ->
            { model | selectedFile = name }



----
--- Helpers
--


getMeta : Model -> Result String Meta
getMeta model =
    model.files
        |> Dict.get metaFileName
        |> Maybe.withDefault ""
        |> MetaFile.stringToMeta
        --         |> Result.mapError (\e -> Types.Error.Simple { pos = -1, kind = Types.Error.Whatever e })
        |> identity


onJust : Maybe (Result e a) -> (a -> Result e b) -> Maybe (Result e b)
onJust maybeResA f =
    case maybeResA of
        Just (Ok a) ->
            f a |> Just

        _ ->
            Nothing



----
--- View
--


viewMaybeRes : String -> (a -> Html msg) -> Maybe (Res a) -> Html msg
viewMaybeRes code f maybeRes =
    case maybeRes of
        Nothing ->
            Html.text ""

        Just res ->
            case Compiler.TestHelpers.resErrorToString res of
                Err e ->
                    Html.code [] [ Html.text e ]

                Ok a ->
                    f a


view : Model -> Html Msg
view model =
    Html.div
        [ class "col" ]
        [ viewFilesSelector model
        , Html.div
            [ class "row thirds" ]
            [ viewSelectedFile model
            , viewProgram model
            , viewTests
            ]
        , Html.node "style" [] [ Html.text Css.css ]
        ]


viewTests : Html Msg
viewTests =
    if not runTests then
        Html.div
            []
            [ Html.h6
                [ style "color" "red" ]
                [ Html.text "TESTS DISABLED" ]
            ]

    else
        Html.div
            [ class "mt ml" ]
            [ tests ]


viewFilesSelector : Model -> Html Msg
viewFilesSelector model =
    let
        viewButton name =
            Html.button
                [ Html.Events.onClick <| OnSelect name
                , Html.Attributes.disabled <| model.selectedFile == name
                , class "ml"
                ]
                [ if name == metaFileName then
                    Html.text <| name ++ ".json"

                  else
                    Html.text <| name ++ ".sp"
                ]
    in
    Html.div
        [ class "row mt"
        ]
        (model.files
            |> Dict.keys
            |> List.filter ((/=) metaFileName)
            |> List.sort
            |> (::) metaFileName
            |> List.map viewButton
        )


viewSelectedFile : Model -> Html Msg
viewSelectedFile model =
    let
        code =
            Dict.get model.selectedFile model.files |> Maybe.withDefault ""
    in
    Html.div
        [ class "col mt ml" ]
        [ viewCodeEditor model code
        , if model.selectedFile == metaFileName then
            viewMeta model code

          else
            viewFileStages model code
        ]


viewCodeEditor : Model -> String -> Html Msg
viewCodeEditor model code =
    let
        meta =
            getMeta model |> Result.withDefault Types.Meta.init

        width =
            code
                |> String.split "\n"
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 10
                |> max 40

        widthAttr =
            width
                |> toFloat
                |> (*) 9
                |> String.fromFloat
                |> (\s -> s ++ "px")
                |> style "width"

        height =
            code
                |> String.split "\n"
                |> List.length
                |> max 15

        heightAttr =
            height
                |> toFloat
                |> (+) 1
                |> (*) 18
                |> String.fromFloat
                |> (\s -> s ++ "px")
                |> style "height"

        viewLineNumber n =
            n
                |> String.fromInt
    in
    Html.div
        [ class "editor"
        , class "row"
        ]
        [ Html.div
            [ class "editor-line-numbers"
            , heightAttr
            ]
            [ List.range 1 height
                |> List.map String.fromInt
                |> String.join "\n"
                |> Html.text
            ]
        , Html.div
            [ class "editor-content"
            , widthAttr
            , heightAttr
            ]
            [ Html.textarea
                [ class "editor-textarea"
                , Html.Events.onInput OnInput
                , Html.Attributes.value code
                , Html.Attributes.spellcheck False
                ]
                [ Html.text code ]
            , Html.div
                [ class "editor-overlay"
                ]
                (viewSyntaxHighlight meta code)
            ]
        ]


viewSyntaxHighlight : Meta -> String -> List (Html msg)
viewSyntaxHighlight meta code =
    case Compiler.StringToTokens.lexer code of
        Err _ ->
            [ Html.text code ]

        Ok tokens ->
            tokens
                |> List.foldl (viewColorToken meta code) ( 0, [] )
                |> Tuple.second
                |> List.reverse


viewColorToken : Meta -> String -> Token -> ( Int, List (Html msg) ) -> ( Int, List (Html msg) )
viewColorToken meta code token ( start, accum ) =
    let
        x =
            Html.span
                [ class (tokenToClass meta token) ]
                [ code
                    |> String.slice start token.end
                    |> Html.text
                ]
    in
    ( token.end, x :: accum )


tokenToClass : Meta -> Token -> String
tokenToClass meta token =
    case token.kind of
        -- Comment
        Token.Comment ->
            "comment"

        -- Terms
        Token.TextLiteral _ ->
            "literal"

        Token.NumberLiteral _ ->
            "literal"

        -- Types
        Token.HasType _ ->
            "op"

        -- Keywords
        Token.Fn ->
            "keyword"

        Token.If ->
            "keyword"

        Token.Try ->
            "keyword"

        Token.As ->
            "keyword"

        Token.Then ->
            "keyword"

        Token.Else ->
            "keyword"

        Token.With ->
            "keyword"

        -- Ops
        Token.Defop _ ->
            "op"

        Token.Unop _ ->
            "op"

        Token.Binop _ _ ->
            "op"

        Token.Arrow _ ->
            "op"

        -- Parens
        Token.RoundParen _ ->
            "paren"

        Token.SquareBracket _ ->
            "paren"

        Token.CurlyBrace _ ->
            "paren"

        Token.Comma ->
            "paren"

        Token.Name _ "alias" ->
            "keyword"

        Token.Name _ "union" ->
            "keyword"

        Token.Name { mutable } name ->
            if mutable then
                "mutable"

            else if Dict.member name meta.globalValues || Dict.member name meta.globalTypes then
                if Compiler.FormattableToCanonicalAst.startsWithUpperChar name then
                    "globalUp"

                else
                    "globalLo"

            else if Compiler.FormattableToCanonicalAst.startsWithUpperChar name then
                "valueUp"

            else
                "valueLo"

        _ ->
            ""


viewMeta : Model -> String -> Html Msg
viewMeta model code =
    case getMeta model of
        Err s ->
            Html.text s

        Ok meta ->
            meta
                |> Debug.toString
                |> Html.text


viewFileStages : Model -> String -> Html Msg
viewFileStages model rawCode =
    let
        code =
            Compiler.TestHelpers.unindent rawCode

        tokens =
            onJust (Just <| Ok code) Compiler.StringToTokens.lexer

        faModule =
            onJust tokens (Compiler.TokensToFormattableAst.parse code model.selectedFile)

        caModule =
            case getMeta model of
                Ok meta ->
                    let
                        ro =
                            { meta = meta
                            , code = code
                            , currentModule = model.selectedFile
                            }
                    in
                    onJust faModule (\fa -> Compiler.FormattableToCanonicalAst.translateModule ro fa Dict.empty)

                Err e ->
                    Just <| Types.Error.errorTodo e
    in
    Html.ul
        []
        [ Html.li
            []
            [ Html.h6 [] [ Html.text "Canonical AST" ]
            , viewMaybeRes code viewCanonicalAst caModule
            ]
        , Html.li
            []
            [ Html.h6 [] [ Html.text "Formattable AST" ]
            , viewMaybeRes code viewFormattableAst faModule
            ]
        , Html.li
            []
            [ Html.h6 [] [ Html.text "Tokens" ]
            , viewMaybeRes code viewTokens tokens
            ]
        ]


viewProgram : Model -> Html Msg
viewProgram model =
    let
        do =
            Lib.result_do

        compileAndInsert : Meta -> String -> String -> CA.AllDefs -> Result String CA.AllDefs
        compileAndInsert meta fileName code acc =
            if fileName == metaFileName then
                Ok acc

            else
                code
                    |> Compiler.TestHelpers.unindent
                    |> (\fa -> Compiler.Pipeline.stringToCanonicalAst meta fileName fa acc)
                    |> Compiler.TestHelpers.resErrorToString

        emitModule : CA.AllDefs -> Result x String
        emitModule caModule =
            caModule
                |> Compiler.CanonicalToJs.translateAll
                |> List.map (Compiler.JsToString.emitStatement 0)
                |> (++) [ Compiler.CanonicalToJs.nativeDefinitions ]
                |> String.join "\n\n"
                |> Ok

        programResult meta =
            do (Lib.dict_foldRes (compileAndInsert meta) model.files Prelude.prelude) <| \allDefs ->
            let
                withAliases : Result String CA.AllDefs
                withAliases =
                    allDefs
                        |> Compiler.ApplyAliases.applyAliasesToModule
                        |> Compiler.TestHelpers.resErrorToString
            in
            do withAliases <| \alsDefs ->
            let
                blah : Result String ( CA.AllDefs, TI.Env, TI.Substitutions )
                blah =
                    alsDefs
                        |> TI.inspectModule Dict.empty
                        |> Compiler.TestHelpers.resErrorToString
            in
            do blah <| \( typedProgram, _, _ ) ->
            Ok typedProgram

        titleAndPreCode title text =
            Html.li
                []
                [ Html.h6 [] [ Html.text title ]
                , Html.pre [] [ Html.code [] [ Html.text text ] ]
                ]
    in
    Html.ul
        [ class "ml mt border"
        , style "padding-right" "1em"
        ]
        (case getMeta model of
            Err e ->
                [ Html.text e ]

            Ok meta ->
                case programResult meta of
                    Err e ->
                        [ Html.pre
                            []
                            [ Html.text e ]
                        ]

                    Ok program ->
                        [ titleAndPreCode
                            "JavaScript value for Mod.result:"
                            (case Compiler.JsToString_Test.runProgram "Main.result" program of
                                Ok res ->
                                    res

                                Err message ->
                                    "Error: ### " ++ message ++ " ###"
                            )
                        , program
                            |> emitModule
                            |> Result.withDefault "error"
                            |> titleAndPreCode "Evaluated JavaScript code:"
                        ]
        )



----
--- Inference
--
-- viewInference : (TI.Eas -> Html msg
{-
   viewInference ( mod, env, subs ) =
       Html.div
           []
           [ env
               |> Dict.toList
               |> List.filter (\( k, v ) -> not (Dict.member k preamble))
               |> List.map (\( k, v ) -> Html.div [] [ k ++ ": " ++ viewSchema v |> Html.text ])
               |> Html.div []

           {-
              , mod.values
                  |> Dict.values
                  --             |> List.sortBy .name
                  |> List.map viewCaDefinition
                  |> Html.code []
           -}
           , subs
               |> Dict.toList
               |> List.map (\( k, v ) -> Html.div [] [ k ++ " => " ++ viewCaType v |> Html.text ])
               |> Html.div []
           ]
-}


viewSchema : TI.EnvEntry -> String
viewSchema schema =
    [ "forall: [" ++ String.join "," (Set.toList schema.forall) ++ "]"
    , "mutable: " ++ Debug.toString schema.mutable
    , "type: " ++ viewCaType schema.type_
    ]
        |> String.join " ### "



{-
   ----
   --- Undeclared
   --


   viewUndeclared : Result (List Compiler.FindUndeclared.Error) Compiler.FindUndeclared.EnvUn -> Html msg
   viewUndeclared un =
       case un of
           Err errors ->
               let
                   viewError error =
                       case error of
                           Compiler.FindUndeclared.ErrorValueUsedBeforeDeclaration name locations ->
                               Html.div [ style "color" "red" ] [ Html.text <| "value `" ++ name ++ "` used before declaration at locations: " ++ Debug.toString locations ]

                           Compiler.FindUndeclared.ErrorUndeclaredTypeVariable name locations ->
                               Html.div [ style "color" "red" ] [ Html.text <| "type variable `" ++ name ++ "` used at " ++ Debug.toString locations ++ " was not declared" ]
               in
               Html.div
                   []
                   (List.map viewError errors)

           Ok m ->
               Html.div
                   []
                   [ Html.div [] [ Html.text <| "types: " ++ Debug.toString m.types ]
                   , Html.div [] [ Html.text <| "values: " ++ Debug.toString m.values ]
                   ]
-}
----
--- Canonical AST
--


viewCanonicalAst : CA.AllDefs -> Html msg
viewCanonicalAst mod =
    let
        v ( name, rv ) =
            case rv of
                CA.Alias a ->
                    viewCaAlias a

                CA.Union u ->
                    viewCaUnion u

                CA.Value d ->
                    viewCaDefinition d
                        |> indentToString
                        |> Html.text
    in
    Html.pre
        []
        (mod
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.map v
        )


viewCaAlias : CA.AliasDef -> Html msg
viewCaAlias al =
    Html.div
        []
        [ [ "alias:"
          , al.name
          , String.join " " al.args
          , "="
          , viewCaType al.ty
          ]
            |> String.join " "
            |> Html.text
        ]


viewCaUnion : CA.UnionDef -> Html msg
viewCaUnion u =
    Html.div
        []
        [ [ "union:"
          , u.name
          , String.join " " u.args
          , "="
          , Debug.toString u.constructors
          ]
            |> String.join " "
            |> Html.text
        ]



----
---


type Indent
    = S String
    | P Indent
    | M (Maybe String)
    | I Indent
    | L (List Indent)


indentToString : Indent -> String
indentToString indent =
    indentToStringRec 0 indent []
        |> List.reverse
        |> String.join "\n"


indentToStringRec : Int -> Indent -> List String -> List String
indentToStringRec cur ind acc =
    let
        shift s =
            String.repeat cur "  " ++ s
    in
    case ind of
        S s ->
            shift s :: acc

        M (Just s) ->
            shift s :: acc

        M Nothing ->
            acc

        P i ->
            case indentToStringRec cur i [] of
                [] ->
                    acc

                [ a ] ->
                    (shift "(" ++ a ++ ")") :: acc

                many ->
                    acc
                        |> (::) (shift "(")
                        |> indentToStringRec (cur + 1) i
                        |> (::) (shift ")")

        I i ->
            indentToStringRec (cur + 1) i acc

        L inds ->
            List.foldl (indentToStringRec cur) acc inds



----


viewCaDefinition : CA.ValueDef -> Indent
viewCaDefinition def =
    L
        [ def.maybeAnnotation
            |> Maybe.map (\x -> viewCaPattern def.pattern ++ " : " ++ viewCaType x)
            |> M
        , S <| viewCaPattern def.pattern ++ " = "
        , I <| L <| List.map viewCaStatement def.body
        ]


viewCaPattern : CA.Pattern -> String
viewCaPattern p =
    case p of
        CA.PatternDiscard _ ->
            "_"

        CA.PatternAny _ n ->
            n

        _ ->
            Debug.toString p


viewCaType : CA.Type -> String
viewCaType ty =
    case ty of
        CA.TypeConstant pos ref args ->
            ref ++ " " ++ String.join " " (List.map viewCaType args)

        CA.TypeVariable pos name ->
            name

        CA.TypeAlias pos path t ->
            "<" ++ path ++ " = " ++ viewCaType t ++ ">"

        CA.TypeFunction pos from fromIsMutable to ->
            [ "(" ++ viewCaType from ++ ")"
            , case fromIsMutable of
                Just True ->
                    " @> "

                Just False ->
                    " -> "

                Nothing ->
                    " ?> "
            , "(" ++ viewCaType to ++ ")"
            ]
                |> String.join ""

        CA.TypeRecord pos extensible attrs ->
            let
                var =
                    case extensible of
                        Just name ->
                            name ++ " with "

                        Nothing ->
                            ""
            in
            attrs
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map (\( name, type_ ) -> name ++ ": " ++ viewCaType type_)
                |> String.join ", "
                |> (\s -> "{" ++ var ++ s ++ "}")


viewCaStatement : CA.Statement -> Indent
viewCaStatement s =
    case s of
        CA.Evaluation expr ->
            P <| viewCaExpression expr

        CA.Definition def ->
            I <| viewCaDefinition def


viewCaExpression : CA.Expression -> Indent
viewCaExpression expr =
    case expr of
        CA.Literal _ (Literal.Text s) ->
            S s

        CA.Literal _ (Literal.Number s) ->
            S s

        CA.Literal _ (Literal.Char s) ->
            S <| Debug.toString s

        CA.Variable _ s ->
            s.name
                :: List.map (\a -> "." ++ a) s.attrPath
                |> String.join ""
                |> S

        CA.Call _ reference argument ->
            L
                [ viewCaExpression reference
                , I
                    (case argument of
                        CA.ArgumentMutable args ->
                            S <| "@" ++ args.name ++ String.join ":" args.attrPath

                        CA.ArgumentExpression e ->
                            viewCaExpression e
                    )
                ]

        CA.Lambda _ param body ->
            P <|
                L <|
                    [ S <| "fn " ++ viewCaPattern param ++ " ="
                    , I <| L <| List.map viewCaStatement body
                    ]

        _ ->
            expr
                |> Debug.toString
                |> S



----
--- Formattable AST
--


viewFormattableAst : FA.Module -> Html msg
viewFormattableAst statements =
    statements
        |> List.map viewFaStatement
        |> Html.div []


viewFaStatement : FA.Statement -> Html msg
viewFaStatement s =
    case s of
        FA.UnionDef td ->
            Html.div
                []
                [ td
                    |> Debug.toString
                    |> (++) "type definition: "
                    |> Html.text
                ]

        FA.TypeAlias td ->
            Html.div
                []
                [ td
                    |> Debug.toString
                    |> (++) "type alias: "
                    |> Html.text
                ]

        FA.Evaluation expr ->
            Html.div
                []
                [ Html.text "Evaluation: "
                , viewFaExpression expr
                ]

        FA.Definition { pattern, maybeAnnotation, body } ->
            Html.div
                []
                [ Html.span
                    []
                    [ Html.text (viewFaPattern pattern)
                    ]
                , maybeAnnotation
                    |> Maybe.map (\x -> " : " ++ Debug.toString x)
                    |> Maybe.withDefault ""
                    |> Html.text
                , body
                    |> List.map viewFaStatement
                    |> Html.div []
                ]


viewFaPattern : FA.Pattern -> String
viewFaPattern p =
    case p of
        FA.PatternAny _ n ->
            n

        _ ->
            Debug.toString p


viewFaExpression : FA.Expression -> Html msg
viewFaExpression expr =
    case expr of
        FA.Literal pos s ->
            Html.text (Debug.toString s)

        FA.Variable pos { isBinop } s ->
            Html.text s

        FA.FunctionCall pos reference arguments ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text "[call] "
                    , viewFaExpression reference
                    ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    (List.map viewFaExpression arguments)
                ]

        FA.Binop pos group sepList ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text <| "[op] " ]

                --TODO ++ op ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    []

                --TODO viewFaExpression left ]
                , Html.div
                    []
                    [ Html.text "---" ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    []

                --TODO viewFaExpression right ]
                ]

        --     Unop String Expression
        _ ->
            Html.code
                []
                [ expr
                    |> Debug.toString
                    |> Html.text
                ]



----
--- Tokens
--


viewTokens : List Token -> Html msg
viewTokens tokens =
    tokens
        |> List.map (\t -> Html.div [] [ Html.text (Debug.toString t.kind) ])
        |> Html.div []



----
---
--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
