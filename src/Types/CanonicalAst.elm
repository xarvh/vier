module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}

import Dict exposing (Dict)
import Types.Literal


type alias AllDefs =
    Dict String RootDef


{-| moduleName + start + end must be unique for every expression, because they are used to build a unique id
-}
type alias Pos =
    { n : String
    , c : String
    , s : Int
    , e : Int
    }


posToUid : Pos -> String
posToUid pos =
    pos.n ++ " " ++ String.fromInt pos.s ++ " " ++ String.fromInt pos.e


posDummy : Pos
posDummy =
    { n = ""
    , c = ""
    , s = -1
    , e = -1
    }



----
--- Root
--


type RootDef
    = Union UnionDef
    | Alias AliasDef
    | Value RootValueDef


type alias AliasDef =
    { name : String
    , args : List String
    , ty : Type
    }


type alias UnionDef =
    { name : String
    , args : List String
    , constructors : Dict String (List Type)
    }


type alias RootValueDef =
    { name : String
    , localName : String
    , pos : Pos
    , maybeAnnotation : Maybe Type
    , isNative : Bool
    , body : List Statement
    }


type alias LocalValueDef =
    { pattern : Pattern
    , mutable : Bool
    , maybeAnnotation : Maybe Type
    , body : List Statement
    }


rootToLocalDef : RootValueDef -> LocalValueDef
rootToLocalDef r =
    { pattern = PatternAny r.pos r.name
    , mutable = False
    , maybeAnnotation = r.maybeAnnotation
    , body = r.body
    }



----
--- Type
--


type Type
    = TypeConstant Pos String (List Type)
    | TypeVariable Pos String
    | TypeFunction Pos Type Bool Type
    | TypeRecord Pos (Maybe String) (Dict String Type)
    | TypeAlias Pos String Type



----
--- Expressions
--


type Statement
    = Definition LocalValueDef
      -- Evaluations are needed for return, mutation and debug
    | Evaluation Expression


type Expression
    = Literal Pos Types.Literal.Value
    | Variable Pos VariableArgs
    | Lambda Pos Parameter (List Statement)
    | Record Pos (Maybe VariableArgs) (Dict String Expression)
    | Call Pos Expression Argument
    | If
        Pos
        -- we use the if also to get lazy ops and compacted compops, so even if the syntax does
        -- not support statement blocks inside if condition, it's useful that the AST can model it.
        { condition : List Statement
        , true : List Statement
        , false : List Statement
        }
    | Try Pos Expression (List ( Pattern, List Statement ))


type Parameter
    = ParameterPattern Pattern
    | ParameterMutable Pos String


type Argument
    = ArgumentExpression Expression
    | ArgumentMutable VariableArgs


type alias VariableArgs =
    { name : String
    , attrPath : List String

    -- True => declared in the root scope
    -- False => declared inside a function's scope
    , isRoot : Bool
    }



----
--- Pattern
--


type Pattern
    = PatternDiscard Pos
    | PatternAny Pos String
    | PatternLiteral Pos Types.Literal.Value
    | PatternConstructor Pos String (List Pattern)
    | PatternRecord Pos (Dict String Pattern)


patternNames : Pattern -> Dict String Pos
patternNames p =
    case p of
        PatternDiscard pos ->
            Dict.empty

        PatternAny pos n ->
            Dict.singleton n pos

        PatternLiteral pos _ ->
            Dict.empty

        PatternConstructor pos path ps ->
            List.foldl (patternNames >> Dict.union) Dict.empty ps

        PatternRecord pos ps ->
            Dict.foldl (\k -> patternNames >> Dict.union) Dict.empty ps


patternPos : Pattern -> Pos
patternPos pa =
    case pa of
        PatternDiscard p ->
            p

        PatternAny p n ->
            p

        PatternLiteral p _ ->
            p

        PatternConstructor p path ps ->
            p

        PatternRecord p ps ->
            p



----
--- RootDef splitters
--


asValue : RootDef -> Maybe RootValueDef
asValue r =
    case r of
        Value v ->
            Just v

        _ ->
            Nothing


asAlias : RootDef -> Maybe AliasDef
asAlias r =
    case r of
        Alias a ->
            Just a

        _ ->
            Nothing


asUnion : RootDef -> Maybe UnionDef
asUnion r =
    case r of
        Union u ->
            Just u

        _ ->
            Nothing


split : AllDefs -> ( Dict String AliasDef, Dict String UnionDef, Dict String RootValueDef )
split =
    let
        part3 n rootDef ( als, uns, vals ) =
            case rootDef of
                Alias a ->
                    ( Dict.insert n a als, uns, vals )

                Union u ->
                    ( als, Dict.insert n u uns, vals )

                Value v ->
                    ( als, uns, Dict.insert n v vals )
    in
    Dict.foldl part3 ( Dict.empty, Dict.empty, Dict.empty )


findValue : String -> AllDefs -> Maybe RootValueDef
findValue name mod =
    case Dict.get name mod of
        Just (Value def) ->
            Just def

        _ ->
            Nothing



----
--- Pos getters
--


typePos : Type -> Pos
typePos ty =
    case ty of
        TypeConstant p _ _ ->
            p

        TypeVariable p _ ->
            p

        TypeFunction p _ _ _ ->
            p

        TypeRecord p _ _ ->
            p

        TypeAlias p _ _ ->
            p



----
--- Crawler
--
{-
   TODO rename extensionFold_* to posFold_*
   TODO have two separate set of functions, one for removing the Pos and the other for building an accumulator.
-}


type Fold
    = FoldExpr Expression
    | FoldType Type
    | FoldPattern Pattern
    | FoldMutParam String
    | FoldRootValueDef RootValueDef


extensionFold_module : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( AllDefs, acc ) -> ( AllDefs, acc )
extensionFold_module f ( a_defs, acc0 ) =
    let
        fold name a_rootDef ( b_defs, accX ) =
            case a_rootDef of
                Alias a_aliasDef ->
                    Tuple.mapFirst
                        (\b_rootDef -> Dict.insert name (Alias b_rootDef) b_defs)
                        (extensionFold_aliasDef f ( a_aliasDef, accX ))

                Union a_unionDef ->
                    Tuple.mapFirst
                        (\b_rootDef -> Dict.insert name (Union b_rootDef) b_defs)
                        (extensionFold_unionDef f ( a_unionDef, accX ))

                Value a_valueDef ->
                    Tuple.mapFirst
                        (\b_rootDef -> Dict.insert name (Value b_rootDef) b_defs)
                        (extensionFold_rootValueDef f ( a_valueDef, accX ))
    in
    Dict.foldl fold ( Dict.empty, acc0 ) a_defs


extensionFold_rootValueDef : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( RootValueDef, acc ) -> ( RootValueDef, acc )
extensionFold_rootValueDef f ( def, acc0 ) =
    let
        ( b_body, acc1 ) =
            extensionFold_block f ( def.body, acc0 )

        ( b_ann, acc2 ) =
            case def.maybeAnnotation of
                Nothing ->
                    ( def.maybeAnnotation, acc1 )

                Just ty ->
                    extensionFold_type f ( ty, acc1 ) |> Tuple.mapFirst Just

        ( b_pos, acc3 ) =
            f (FoldRootValueDef def) ( def.pos, acc2 )
    in
    ( { name = def.name
      , localName = def.localName
      , pos = b_pos
      , isNative = def.isNative
      , maybeAnnotation = b_ann
      , body = b_body
      }
    , acc3
    )


extensionFold_aliasDef : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( AliasDef, acc ) -> ( AliasDef, acc )
extensionFold_aliasDef f ( def, acc ) =
    ( def.ty, acc )
        |> extensionFold_type f
        |> Tuple.mapFirst
            (\b_ty ->
                { name = def.name
                , args = def.args
                , ty = b_ty
                }
            )


extensionFold_unionDef : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( UnionDef, acc ) -> ( UnionDef, acc )
extensionFold_unionDef f ( def, acc ) =
    let
        foldConstructor constructorName a_constructorArgs ( b_constructors, accX ) =
            a_constructorArgs
                |> List.foldr foldArgument ( [], accX )
                |> Tuple.mapFirst (\b_constructorArgs -> Dict.insert constructorName b_constructorArgs b_constructors)

        foldArgument a_arg ( b_args, accX ) =
            extensionFold_type f ( a_arg, accX )
                |> Tuple.mapFirst (\b_arg -> b_arg :: b_args)
    in
    def.constructors
        |> Dict.foldl foldConstructor ( Dict.empty, acc )
        |> Tuple.mapFirst
            (\b_constructors ->
                { name = def.name
                , args = def.args
                , constructors = b_constructors
                }
            )


extensionFold_valueDef : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( LocalValueDef, acc ) -> ( LocalValueDef, acc )
extensionFold_valueDef f ( def, acc0 ) =
    let
        ( b_body, acc1 ) =
            extensionFold_block f ( def.body, acc0 )

        ( b_ann, acc2 ) =
            case def.maybeAnnotation of
                Nothing ->
                    ( def.maybeAnnotation, acc1 )

                Just ty ->
                    extensionFold_type f ( ty, acc1 ) |> Tuple.mapFirst Just

        ( b_pattern, acc3 ) =
            extensionFold_pattern f ( def.pattern, acc2 )
    in
    ( { pattern = b_pattern
      , mutable = def.mutable
      , maybeAnnotation = b_ann
      , body = b_body
      }
    , acc3
    )


extensionFold_type : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( Type, acc ) -> ( Type, acc )
extensionFold_type f ( ty, acc0 ) =
    let
        fty =
            f (FoldType ty)
    in
    case ty of
        TypeConstant a_pos name a_args ->
            let
                ( b_pos, acc1 ) =
                    fty ( a_pos, acc0 )

                ( b_args, acc2 ) =
                    List.foldr fold ( [], acc1 ) a_args

                fold a_arg ( args, accX ) =
                    extensionFold_type f ( a_arg, accX )
                        |> Tuple.mapFirst (\b_arg -> b_arg :: args)
            in
            ( TypeConstant b_pos name b_args
            , acc2
            )

        TypeVariable a_pos name ->
            fty ( a_pos, acc0 )
                |> Tuple.mapFirst (\b_pos -> TypeVariable b_pos name)

        TypeFunction a_pos a_from fromIsMut a_to ->
            let
                ( b_pos, acc1 ) =
                    fty ( a_pos, acc0 )

                ( b_from, acc2 ) =
                    extensionFold_type f ( a_from, acc1 )

                ( b_to, acc3 ) =
                    extensionFold_type f ( a_to, acc2 )
            in
            ( TypeFunction b_pos b_from fromIsMut b_to
            , acc3
            )

        TypeRecord a_pos ext a_attrs ->
            let
                ( b_pos, acc1 ) =
                    fty ( a_pos, acc0 )

                fold attr_name a_ty ( attrs, accX ) =
                    Tuple.mapFirst
                        (\b_arg -> Dict.insert attr_name b_arg attrs)
                        (extensionFold_type f ( a_ty, accX ))

                ( b_attrs, acc2 ) =
                    Dict.foldl fold ( Dict.empty, acc1 ) a_attrs
            in
            ( TypeRecord b_pos ext b_attrs
            , acc2
            )

        TypeAlias a_pos name a_ty ->
            let
                ( b_pos, acc1 ) =
                    fty ( a_pos, acc0 )

                ( b_ty, acc2 ) =
                    extensionFold_type f ( a_ty, acc1 )
            in
            ( TypeAlias b_pos name b_ty
            , acc2
            )


extensionFold_expression : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( Expression, acc ) -> ( Expression, acc )
extensionFold_expression fFold ( expr, acc ) =
    let
        f =
            FoldExpr >> fFold
    in
    case expr of
        Literal a_pos ar ->
            Tuple.mapFirst (\b_pos -> Literal b_pos ar) (f expr ( a_pos, acc ))

        Variable a_pos args ->
            Tuple.mapFirst (\b_pos -> Variable b_pos args) (f expr ( a_pos, acc ))

        Lambda a_pos a_param a_body ->
            let
                ( b_pos, acc1 ) =
                    f expr ( a_pos, acc )

                ( b_body, acc2 ) =
                    extensionFold_block fFold ( a_body, acc1 )

                ( b_param, acc3 ) =
                    case a_param of
                        ParameterMutable pos name ->
                            fFold (FoldMutParam name) ( pos, acc2 )
                                |> Tuple.mapFirst (\p -> ParameterMutable p name)

                        ParameterPattern a_pattern ->
                            extensionFold_pattern fFold ( a_pattern, acc2 )
                                |> Tuple.mapFirst ParameterPattern
            in
            ( Lambda b_pos b_param b_body
            , acc3
            )

        Record a_pos a_ext a_attrs ->
            let
                ( b_pos, acc1 ) =
                    f expr ( a_pos, acc )

                fold name a_expr ( attrs, aX ) =
                    Tuple.mapFirst
                        (\b_expr -> Dict.insert name b_expr attrs)
                        (extensionFold_expression fFold ( a_expr, aX ))

                ( b_attrs, acc2 ) =
                    Dict.foldl fold ( Dict.empty, acc1 ) a_attrs
            in
            ( Record b_pos a_ext b_attrs
            , acc2
            )

        Call a_pos a_ref a_arg ->
            let
                ( b_pos, acc1 ) =
                    f expr ( a_pos, acc )

                ( b_ref, acc2 ) =
                    extensionFold_expression fFold ( a_ref, acc1 )

                ( b_arg, acc3 ) =
                    extensionFold_argument fFold ( a_arg, acc2 )
            in
            ( Call b_pos b_ref b_arg
            , acc3
            )

        If a_pos ar ->
            let
                -- TODO use a `do blah <| \b_meh ->` to avoid using accX?
                ( b_pos, acc1 ) =
                    f expr ( a_pos, acc )

                ( b_cond, acc2 ) =
                    extensionFold_block fFold ( ar.condition, acc1 )

                ( b_true, acc3 ) =
                    extensionFold_block fFold ( ar.true, acc2 )

                ( b_false, acc4 ) =
                    extensionFold_block fFold ( ar.false, acc3 )
            in
            ( If b_pos { condition = b_cond, true = b_true, false = b_false }
            , acc4
            )

        Try a_pos a_value a_tries ->
            let
                ( b_pos, acc1 ) =
                    f expr ( a_pos, acc )

                ( b_value, acc2 ) =
                    extensionFold_expression fFold ( a_value, acc1 )

                fold ( a_pattern, a_block ) ( pasAndBlocks, accX0 ) =
                    let
                        ( b_block, accX1 ) =
                            extensionFold_block fFold ( a_block, accX0 )

                        ( b_pattern, accX2 ) =
                            extensionFold_pattern fFold ( a_pattern, accX1 )
                    in
                    ( ( b_pattern, b_block ) :: pasAndBlocks, accX2 )

                ( b_patterns, acc3 ) =
                    List.foldr fold ( [], acc2 ) a_tries
            in
            ( Try b_pos b_value b_patterns
            , acc3
            )


extensionFold_pattern : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( Pattern, acc ) -> ( Pattern, acc )
extensionFold_pattern fFold ( pattern, acc ) =
    let
        f =
            FoldPattern >> fFold
    in
    case pattern of
        PatternDiscard a_pos ->
            Tuple.mapFirst (\b_pos -> PatternDiscard b_pos) (f pattern ( a_pos, acc ))

        PatternAny a_pos name ->
            Tuple.mapFirst (\b_pos -> PatternAny b_pos name) (f pattern ( a_pos, acc ))

        PatternLiteral a_pos value ->
            Tuple.mapFirst (\b_pos -> PatternLiteral b_pos value) (f pattern ( a_pos, acc ))

        PatternConstructor a_pos name a_args ->
            let
                ( b_pos, acc1 ) =
                    f pattern ( a_pos, acc )

                fold a_arg ( b_args, accX ) =
                    extensionFold_pattern fFold ( a_arg, accX )
                        |> Tuple.mapFirst (\b_arg -> b_arg :: b_args)

                ( b_as, acc2 ) =
                    List.foldr fold ( [], acc1 ) a_args
            in
            ( PatternConstructor b_pos name b_as
            , acc2
            )

        PatternRecord a_pos a_attrs ->
            let
                ( b_pos, acc1 ) =
                    f pattern ( a_pos, acc )

                fold name a_pa ( attrs, aX ) =
                    Tuple.mapFirst
                        (\b_pa -> Dict.insert name b_pa attrs)
                        (extensionFold_pattern fFold ( a_pa, aX ))

                ( b_attrs, acc2 ) =
                    Dict.foldl fold ( Dict.empty, acc1 ) a_attrs
            in
            ( PatternRecord b_pos b_attrs
            , acc2
            )


extensionFold_block : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( List Statement, acc ) -> ( List Statement, acc )
extensionFold_block f ( block, acc ) =
    let
        fold a_stat ( b_stats, accX ) =
            Tuple.mapFirst
                (\b_stat -> b_stat :: b_stats)
                (extensionFold_statement f ( a_stat, accX ))
    in
    List.foldr fold ( [], acc ) block


extensionFold_argument : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( Argument, acc ) -> ( Argument, acc )
extensionFold_argument f ( arg, acc ) =
    case arg of
        ArgumentExpression expr ->
            ( expr, acc )
                |> extensionFold_expression f
                |> Tuple.mapFirst ArgumentExpression

        ArgumentMutable ar ->
            ( arg, acc )


extensionFold_statement : (Fold -> ( Pos, acc ) -> ( Pos, acc )) -> ( Statement, acc ) -> ( Statement, acc )
extensionFold_statement f ( stat, acc ) =
    case stat of
        Definition ar ->
            Tuple.mapFirst Definition (extensionFold_valueDef f ( ar, acc ))

        Evaluation expr ->
            ( expr, acc )
                |> extensionFold_expression f
                |> Tuple.mapFirst Evaluation
