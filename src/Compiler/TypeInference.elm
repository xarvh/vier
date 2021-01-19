module Compiler.TypeInference exposing (..)

import Dict exposing (Dict)
import Generator as TyGen
import Html
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Name, Type)


type alias Res a =
    Result String a


type alias Substitutions =
    Dict Name Type


type alias Env =
    Dict Name EnvEntry


{-| TODO rename to Schema?
-}
type alias EnvEntry =
    { type_ : Type

    {- TODO
       When you have forall you don't have mutable?

       type Schema
         = Forall (Set name)
         | Mutable
    -}
    , forall : Set Name

    -- TODO: this field should contain a WHY that explains why a variable is or is not mutable
    -- so that we can show a clear explanation to the user
    , mutable : Maybe Bool
    }



----
--- Variable type generator
--


type alias TyGen a =
    TyGen.Generator Int a


newName : TyGen Name
newName =
    TyGen.next ((+) 1) (\n -> "t" ++ String.fromInt n)


newType : TyGen Type
newType =
    TyGen.map (\s -> CA.TypeVariable { name = s }) newName


do_nr : TyGen (Res a) -> (a -> TyGen (Res b)) -> TyGen (Res b)
do_nr nra f =
    TyGen.do nra
        (\ra ->
            case ra of
                Ok a ->
                    f a

                Err e ->
                    TyGen.wrap (Err e)
        )


do_res a b =
    Result.andThen b a


nr_map : (a -> b) -> TyGen (Res a) -> TyGen (Res b)
nr_map f tr =
    TyGen.map (Result.map f) tr



----
--- Core types
--


typeNone : Type
typeNone =
    CA.TypeConstant { path = "None", args = [] }



----
--- Modules
--


inspectModule : Env -> CA.Module e -> Res Env
inspectModule prelude mod =
    do_res (Dict.foldl addConstructors (Ok prelude) mod.types) <| \env ->
    let
        --         _ =
        --             Debug.log "" env
        statements =
            mod.values
                |> Dict.values
                |> List.map CA.Definition

        gen =
            do_nr (inspectStatementList statements env Dict.empty) <| \( shouldBeNone, env1, subs ) ->
            refine_env subs env1
                |> Ok
                |> TyGen.wrap

        ( envResult, nextId ) =
            TyGen.run 0 gen
    in
    envResult


addConstructors : String -> CA.TypeDefinition -> Res Env -> Res Env
addConstructors _ def resEnv =
    do_res resEnv <| \env ->
    addConstructorsRec def def.constructors env


addConstructorsRec : CA.TypeDefinition -> List CA.TypeConstructor -> Env -> Res Env
addConstructorsRec typeDef constructors env =
    case constructors of
        [] ->
            Ok env

        ctor :: cTail ->
            let
                args =
                    List.map (\a -> CA.TypeVariable { name = a }) typeDef.args

                ctorType =
                    -- TODO ensure that all constructors use declared var types?
                    -- ^ This is something that should be done by FormattableToCanonical?
                    List.foldr fold (CA.TypeConstant { path = typeDef.name, args = args }) ctor.args

                fold ty accum =
                    CA.TypeFunction
                        { from = ty
                        , fromIsMutable = Just False
                        , to = accum
                        }
            in
            case validateType False ctorType of
                Just err ->
                    Err err

                Nothing ->
                    env
                        |> Dict.insert ctor.name
                            { type_ = ctorType
                            , forall = Set.empty
                            , mutable = Just False
                            }
                        |> addConstructorsRec typeDef cTail



----
--- Substitutions
--


refine_type : Substitutions -> Type -> Type
refine_type subs ty =
    case ty of
        CA.TypeConstant { path, args } ->
            CA.TypeConstant { path = path, args = List.map (refine_type subs) args }

        CA.TypeVariable { name } ->
            case Dict.get name subs of
                Just substitutionType ->
                    -- a substitution exists for the variable type v
                    refine_type subs substitutionType

                Nothing ->
                    -- no substitution, return the type as-is
                    ty

        CA.TypeFunction { from, fromIsMutable, to } ->
            CA.TypeFunction
                { from = refine_type subs from
                , fromIsMutable = fromIsMutable
                , to = refine_type subs to
                }

        CA.TypeRecord args ->
            case args.extensible |> Maybe.andThen (\name -> Dict.get name subs) of
                Nothing ->
                    CA.TypeRecord
                        { extensible = args.extensible
                        , attrs = Dict.map (\name -> refine_type subs) args.attrs
                        }

                Just substitutionType ->
                    substitutionType


tyvars_from_type : Type -> Set Name
tyvars_from_type ty =
    case ty of
        CA.TypeVariable { name } ->
            Set.singleton name

        CA.TypeFunction { from, to } ->
            Set.union (tyvars_from_type from) (tyvars_from_type to)

        CA.TypeConstant { path, args } ->
            List.foldl (\a -> Set.union (tyvars_from_type a)) Set.empty args

        CA.TypeRecord args ->
            let
                init =
                    case args.extensible of
                        Nothing ->
                            Set.empty

                        Just name ->
                            Set.singleton name
            in
            Dict.foldl (\n t -> Set.union (tyvars_from_type t)) init args.attrs



----
--- Env
--


instantiate_type : Type -> Set Name -> TyGen Type
instantiate_type t tvars =
    let
        aggregate : Name -> TyGen Substitutions -> TyGen Substitutions
        aggregate tvar nextSubsAcc =
            TyGen.do newType <| \nt ->
            TyGen.map (Dict.insert tvar nt) nextSubsAcc

        genSubs : TyGen Substitutions
        genSubs =
            Set.foldl aggregate (TyGen.wrap Dict.empty) tvars
    in
    TyGen.map (\subs -> refine_type subs t) genSubs


env_get : Name -> Env -> TyGen (Res Type)
env_get v e =
    case Dict.get v e of
        Just { type_, forall, mutable } ->
            instantiate_type type_ forall
                |> TyGen.map Ok

        Nothing ->
            ("unbound variable: " ++ v)
                |> Err
                |> TyGen.wrap


refine_env : Substitutions -> Env -> Env
refine_env s env =
    let
        refine_entry _ entry =
            { entry | type_ = refine_type (Set.foldl Dict.remove s entry.forall) entry.type_ }
    in
    Dict.map refine_entry env


unify : Type -> Type -> Substitutions -> TyGen (Res Substitutions)
unify t1 t2 s =
    let
        t1_refined =
            refine_type s t1

        t2_refined =
            refine_type s t2

        cycle v t =
            Set.member v (tyvars_from_type t)
    in
    case ( t1_refined, t2_refined ) of
        ( CA.TypeConstant c1, CA.TypeConstant c2 ) ->
            if c1.path /= c2.path then
                TyGen.wrap <| Err <| "cannot unify " ++ c1.path ++ " and " ++ c2.path

            else
                let
                    rec a1 a2 subs =
                        case ( a1, a2 ) of
                            ( [], [] ) ->
                                TyGen.wrap <| Ok subs

                            ( head1 :: tail1, head2 :: tail2 ) ->
                                do_nr (unify head1 head2 subs) <| rec tail1 tail2

                            _ ->
                                TyGen.wrap <| Err <| "one of the two has wrong number of args: " ++ c1.path ++ " and " ++ c2.path
                in
                rec c1.args c2.args s

        ( CA.TypeVariable v1, CA.TypeVariable v2 ) ->
            TyGen.wrap
                (if v1 == v2 then
                    Ok s

                 else
                    s
                        |> Dict.insert v1.name t2_refined
                        |> Ok
                )

        ( CA.TypeVariable v1, _ ) ->
            TyGen.wrap
                (if cycle v1.name t2 then
                    -- is this the correct behavior?
                    Err "cycle!"

                 else
                    s
                        |> Dict.insert v1.name t2_refined
                        |> Ok
                )

        ( _, CA.TypeVariable v2 ) ->
            unify t2_refined t1_refined s

        ( CA.TypeFunction a, CA.TypeFunction b ) ->
            let
                maybeClash =
                    Maybe.map2 (\aa bb -> aa /= bb) a.fromIsMutable b.fromIsMutable
            in
            if Maybe.withDefault False maybeClash then
                TyGen.wrap <| Err <| "mutability clash: " ++ Debug.toString t1_refined ++ " and " ++ Debug.toString t2_refined

            else
                do_nr (unify a.to b.to s) <| unify a.from b.from

        ( CA.TypeRecord a, CA.TypeRecord b ) ->
            unifyRecords a b s

        _ ->
            TyGen.wrap <| Err <| "cannot unify " ++ Debug.toString t1_refined ++ " and " ++ Debug.toString t2_refined


type alias UnifyRecordsFold =
    { aOnly : Dict Name Type
    , bOnly : Dict Name Type
    , both : Dict Name ( Type, Type )
    }


unifyRecords : CA.TypeRecordArgs -> CA.TypeRecordArgs -> Substitutions -> TyGen (Res Substitutions)
unifyRecords aArgs bArgs subs0 =
    -- TODO if aArg == bArg do nothing
    let
        initState : UnifyRecordsFold
        initState =
            { aOnly = Dict.empty
            , bOnly = Dict.empty
            , both = Dict.empty
            }

        onA : Name -> Type -> UnifyRecordsFold -> UnifyRecordsFold
        onA name type_ state =
            { state | aOnly = Dict.insert name type_ state.aOnly }

        onB : Name -> Type -> UnifyRecordsFold -> UnifyRecordsFold
        onB name type_ state =
            { state | bOnly = Dict.insert name type_ state.bOnly }

        onBoth : Name -> Type -> Type -> UnifyRecordsFold -> UnifyRecordsFold
        onBoth name aType bType state =
            { state | both = Dict.insert name ( aType, bType ) state.both }

        { aOnly, bOnly, both } =
            Dict.merge onA onBoth onB aArgs.attrs bArgs.attrs initState

        unifyBothRec subs ls =
            case ls of
                [] ->
                    TyGen.wrap <| Ok subs

                ( name, ( aType, bType ) ) :: tail ->
                    do_nr (unify aType bType subs) <| \subsN ->
                    unifyBothRec subsN tail

        tyResSubs1 =
            both
                |> Dict.toList
                |> unifyBothRec subs0
    in
    if aOnly == Dict.empty && bOnly == Dict.empty then
        tyResSubs1

    else
        do_nr tyResSubs1 <| \subs1 ->
        -- from this point on, we can assume that the common attributes are compatible
        case ( aArgs.extensible, bArgs.extensible ) of
            ( Just aName, Nothing ) ->
                if aOnly /= Dict.empty then
                    "a has arguments that do not exist in b"
                        |> Err
                        |> TyGen.wrap

                else
                    -- substitute a with b
                    Dict.insert aName (CA.TypeRecord bArgs) subs1
                        |> Ok
                        |> TyGen.wrap

            ( Nothing, Just bName ) ->
                if bOnly /= Dict.empty then
                    "b has arguments that do not exist in a"
                        |> Err
                        |> TyGen.wrap

                else
                    -- substitute b with a
                    Dict.insert bName (CA.TypeRecord aArgs) subs1
                        |> Ok
                        |> TyGen.wrap

            ( Nothing, Nothing ) ->
                if bOnly == Dict.empty && aOnly == Dict.empty then
                    -- the two are the same
                    subs1
                        |> Ok
                        |> TyGen.wrap

                else
                    "the two records are just too different"
                        |> Err
                        |> TyGen.wrap

            ( Just aName, Just bName ) ->
                TyGen.do newName <| \new ->
                let
                    subTy =
                        CA.TypeRecord { extensible = Just new, attrs = Dict.union bOnly aArgs.attrs }
                in
                subs1
                    |> Dict.insert aName subTy
                    |> Dict.insert bName subTy
                    |> Ok
                    |> TyGen.wrap


literalToType : literal -> Type
literalToType l =
    -- TODO
    CA.TypeConstant { path = "Number", args = [] }


generalize : Name -> Env -> Type -> Set Name
generalize name env ty =
    let
        fold k schema acc =
            -- don't add the value's own tyvars!
            if k == name then
                acc

            else
                Set.union (Set.diff (tyvars_from_type schema.type_) schema.forall) acc

        tyvarsFromEnv : Set Name
        tyvarsFromEnv =
            Dict.foldl fold Set.empty env
    in
    Set.diff (tyvars_from_type ty) tyvarsFromEnv



----
--- Inspect
--


type alias Eas =
    ( Env, Substitutions )


andEnv : Env -> TyGen (Res Substitutions) -> TyGen (Res Eas)
andEnv env =
    nr_map (\subs -> ( env, subs ))


unifyWithAttrPath : List Name -> Type -> Type -> Substitutions -> TyGen (Res Substitutions)
unifyWithAttrPath attrPath typeAtPathEnd valueType subs =
    case attrPath of
        [] ->
            unify typeAtPathEnd valueType subs

        head :: tail ->
            TyGen.do newName <| \n1 ->
            TyGen.do newType <| \t1 ->
            let
                -- `rType` : { n1 | `head` : t1 }
                rType =
                    CA.TypeRecord
                        { extensible = Just n1
                        , attrs = Dict.singleton head t1
                        }
            in
            do_nr (unify rType valueType subs) <| \subs1 ->
            unifyWithAttrPath tail typeAtPathEnd t1 subs1


{-| TODO move Env at the end?
-}
inspect_expr : Env -> CA.Expression e -> Type -> Substitutions -> TyGen (Res Eas)
inspect_expr env expr ty subs =
    case expr of
        CA.NumberLiteral _ l ->
            subs
                |> unify ty (literalToType l)
                |> andEnv env

        CA.Variable _ { path, attrPath } ->
            -- Every time I use a var with variable type, it should be instantiated,
            -- because each time it may by used against a different type.
            -- This is done automatically by `env_get`.
            do_nr (env_get path env) <| \nt ->
            let
                t =
                    refine_type subs nt
            in
            subs
                |> unifyWithAttrPath attrPath ty t
                |> andEnv env

        CA.Lambda _ args ->
            if Dict.member args.parameter env then
                ("function parameter `" ++ args.parameter ++ "` shadows env variable")
                    |> Err
                    |> TyGen.wrap

            else
                TyGen.do newType <| \v_t ->
                TyGen.do newType <| \e_t ->
                let
                    env1 =
                        Dict.insert args.parameter { type_ = v_t, forall = Set.empty, mutable = Nothing } env
                in
                do_nr (inspectStatementList args.body env1 subs) <| \( returnType, env2, subs1 ) ->
                let
                    fromIsMutable =
                        case Dict.get args.parameter env2 of
                            Nothing ->
                                Debug.todo "I'm pretty sure the arg IS in the env"

                            Just schema ->
                                schema.mutable
                in
                do_nr (unify ty (CA.TypeFunction { from = v_t, fromIsMutable = fromIsMutable, to = e_t }) subs1) <| \subs2 ->
                unify e_t returnType subs2
                    |> andEnv env

        CA.Call _ args ->
            TyGen.do newType <| \e_t ->
            do_nr (inspect_argument env args.argument e_t subs) <| \( env1, subs1 ) ->
            let
                fromIsMutable =
                    case args.argument of
                        CA.ArgumentMutable _ ->
                            True

                        CA.ArgumentExpression _ ->
                            False

                f_t =
                    CA.TypeFunction { from = e_t, fromIsMutable = Just fromIsMutable, to = ty }

                f_t1 =
                    refine_type subs1 f_t
            in
            inspect_expr (refine_env subs1 env1) args.reference f_t1 subs1

        CA.If _ _ ->
            ("inference NI: " ++ Debug.toString expr)
                |> Err
                |> TyGen.wrap

        CA.Record _ args ->
            let
                init =
                    ( Dict.empty, ( env, subs ) )
                        |> Ok
                        |> TyGen.wrap

                foldAttr :
                    Name
                    -> CA.Expression e
                    -> TyGen (Res ( Dict Name Type, Eas ))
                    -> TyGen (Res ( Dict Name Type, Eas ))
                foldAttr attrName attrValue genResAccum =
                    do_nr genResAccum <| \( attrsAccum, ( envAccum, subsAccum ) ) ->
                    TyGen.do newType <| \nt ->
                    do_nr (inspect_expr envAccum attrValue nt subsAccum) <| \eas ->
                    ( Dict.insert attrName nt attrsAccum, eas )
                        |> Ok
                        |> TyGen.wrap
            in
            do_nr (Dict.foldr foldAttr init args.attrs) <| \( attrTypes, ( env1, subs1 ) ) ->
            do_nr (inspect_maybeUpdateTarget env1 args.maybeUpdateTarget ty subs1) <| \( extensible, ( env2, subs2 ) ) ->
            let
                refinedAttrTypes =
                    -- first I need all new subs, only then it makes sense to apply them
                    Dict.map (\attrName attrType -> refine_type subs2 attrType) attrTypes
            in
            subs2
                |> unify ty (CA.TypeRecord { extensible = extensible, attrs = refinedAttrTypes })
                |> andEnv env2


inspect_maybeUpdateTarget : Env -> Maybe CA.VariableArgs -> Type -> Substitutions -> TyGen (Res ( Maybe Name, Eas ))
inspect_maybeUpdateTarget env maybeUpdateTarget ty subs =
    case maybeUpdateTarget of
        Nothing ->
            TyGen.wrap <| Ok <| ( Nothing, ( env, subs ) )

        Just updateTarget ->
            TyGen.do newName <| \n ->
            inspect_expr env (CA.Variable () updateTarget) ty subs
                |> nr_map (\eas -> ( Just n, eas ))


inspect_argument : Env -> CA.Argument e -> Type -> Substitutions -> TyGen (Res Eas)
inspect_argument env arg ty subs =
    case arg of
        CA.ArgumentMutable { path, attrPath } ->
            case Dict.get path env of
                Nothing ->
                    ("undeclared mutable variable: " ++ path)
                        |> Err
                        |> TyGen.wrap

                Just schema ->
                    case schema.mutable of
                        Nothing ->
                            unifyWithAttrPath attrPath ty schema.type_ subs
                                |> nr_map (\s -> ( Dict.insert path { schema | mutable = Just True } env, s ))

                        Just True ->
                            unifyWithAttrPath attrPath ty schema.type_ subs
                                |> nr_map (\s -> ( env, s ))

                        Just False ->
                            (path ++ " can't be mutable")
                                |> Err
                                |> TyGen.wrap

        CA.ArgumentExpression expr ->
            inspect_expr env expr ty subs



----
---
--


inspect_statement : CA.Statement e -> Env -> Substitutions -> TyGen (Res ( Type, Env, Substitutions ))
inspect_statement statement env subs =
    case statement of
        CA.Evaluation expr ->
            TyGen.do newType <| \nt ->
            do_nr (inspect_expr env expr nt subs) <| \( env1, subs1 ) ->
            let
                refinedNt =
                    refine_type subs1 nt
            in
            ( refinedNt, env1, subs1 )
                |> Ok
                |> TyGen.wrap

        CA.Definition { name, mutable, body, maybeAnnotation } ->
            do_nr (inspectStatementList body env subs) <| \( returnType, _, subs1 ) ->
            case Dict.get name env of
                Nothing ->
                    Debug.todo "WTF dict should contain def name already"

                Just def ->
                    do_nr (unify returnType def.type_ subs1) <| \subs2 ->
                    let
                        refinedType =
                            refine_type subs2 def.type_

                        -- https://cstheory.stackexchange.com/questions/42554/extending-hindley-milner-to-type-mutable-references
                        -- This is also the reason why we can't infer whether a value is mutable or not
                        forall =
                            if mutable then
                                Set.empty

                            else
                                generalize name (refine_env subs2 env) refinedType

                        scheme : EnvEntry
                        scheme =
                            { type_ = refinedType
                            , forall = forall
                            , mutable = Just mutable
                            }

                        env1 =
                            Dict.insert name scheme env
                    in
                    case annotationTooGeneral maybeAnnotation forall of
                        Nothing ->
                            -- A definition has no type
                            Ok ( typeNone, env1, subs2 )
                                |> TyGen.wrap

                        Just error ->
                            Err error
                                |> TyGen.wrap


annotationTooGeneral : Maybe Type -> Set Name -> Maybe String
annotationTooGeneral maybeAnnotation inferredForall =
    case maybeAnnotation of
        Nothing ->
            Nothing

        Just annotation ->
            let
                -- This is already calculated when we add the raw definitions to env
                -- Is it faster to get it from env?
                annotationForall =
                    tyvars_from_type annotation
            in
            if Set.size annotationForall > Set.size inferredForall then
                Just <| "annotation too general : " ++ Debug.toString annotationForall ++ " vs " ++ Debug.toString inferredForall

            else
                Nothing



----
--- Definition Body
--


inspectStatementList : List (CA.Statement e) -> Env -> Substitutions -> TyGen (Res ( Type, Env, Substitutions ))
inspectStatementList stats parentEnv subs =
    let
        definitionOrStatement stat =
            case stat of
                CA.Definition d ->
                    Left d

                _ ->
                    Right stat

        -- A statement list can contain definitions, creating its own scope
        -- Definitions can be recursive and in general appear in any order, so we want to add them to the environment before we inspect them
        ( definitions, nonDefs ) =
            partition definitionOrStatement stats

        definedMutables =
            List.filter .mutable definitions

        -- Also, we need to reorder them, so that dependent sibling defs come after
        orderedDefinitions =
            reorderDefinitions definitions

        newStats =
            List.map CA.Definition orderedDefinitions ++ nonDefs
    in
    do_nr (insertDefinitionRec definitions parentEnv) <| \localEnv ->
    do_nr (inspectStatementRec newStats typeNone localEnv subs) <| \typeAndEnvAndSubs ->
    TyGen.wrap <|
        let
            ( ty, env, _ ) =
                typeAndEnvAndSubs

            defContainsFunctions : CA.ValueDefinition e -> Bool
            defContainsFunctions def =
                case Dict.get def.name env of
                    Nothing ->
                        Debug.todo "NAHAHHAHAHAHAHAHA"

                    Just schema ->
                        typeContainsFunctions schema.type_

            mutablesWithFunction =
                List.filter defContainsFunctions definedMutables
        in
        if mutablesWithFunction /= [] then
            mutablesWithFunction
                |> List.map .name
                |> String.join ", "
                |> (++) "these mutable values contain functions: "
                |> Err

        else if definedMutables /= [] && typeContainsFunctions ty then
            Err "statement blocks that define mutables can't return functions"

        else
            Ok typeAndEnvAndSubs


inspectStatementRec : List (CA.Statement e) -> Type -> Env -> Substitutions -> TyGen (Res ( Type, Env, Substitutions ))
inspectStatementRec stats returnType env subs =
    case stats of
        [] ->
            ( returnType, env, subs )
                |> Ok
                |> TyGen.wrap

        stat :: statsTail ->
            do_nr (inspect_statement stat env subs) <| \( ty, env1, subs1 ) ->
            inspectStatementRec statsTail ty env1 subs1


insertDefinitionRec : List (CA.ValueDefinition e) -> Env -> TyGen (Res Env)
insertDefinitionRec defs env =
    case defs of
        [] ->
            env
                |> Ok
                |> TyGen.wrap

        def :: ds ->
            if Dict.member def.name env then
                (def.name ++ " already declared in scope!")
                    |> Err
                    |> TyGen.wrap

            else
                case def.maybeAnnotation of
                    Just annotation ->
                        case validateType def.mutable annotation of
                            Just err ->
                                err
                                    |> Err
                                    |> TyGen.wrap

                            Nothing ->
                                env
                                    |> Dict.insert def.name
                                        { type_ = annotation

                                        -- TODO remove parent annotation tyvars!
                                        , forall = tyvars_from_type annotation
                                        , mutable = Just def.mutable
                                        }
                                    |> insertDefinitionRec ds

                    Nothing ->
                        TyGen.do newName <| \name ->
                        env
                            |> Dict.insert def.name
                                { type_ = CA.TypeVariable { name = name }
                                , forall = Set.singleton name
                                , mutable = Just def.mutable
                                }
                            |> insertDefinitionRec ds


statementAsDefinition : CA.Statement e -> Maybe (CA.ValueDefinition e)
statementAsDefinition stat =
    case stat of
        CA.Definition d ->
            Just d

        _ ->
            Nothing


validateType : Bool -> Type -> Maybe String
validateType mutable ty =
    case ty of
        CA.TypeConstant _ ->
            Nothing

        CA.TypeVariable { name } ->
            if mutable then
                Just "variable types can't be mutable"

            else
                Nothing

        CA.TypeFunction { from, fromIsMutable, to } ->
            if mutable then
                Just "mutable values can't contain functions"

            else
                case validateType (Maybe.withDefault False fromIsMutable) from of
                    Just e ->
                        Just e

                    Nothing ->
                        validateType False to

        CA.TypeRecord args ->
            args.attrs
                -- TODO Rewrite the whole dumpster fire to support returning  multiple errors
                |> Dict.values
                |> List.filterMap (validateType mutable)
                |> List.head


typeContainsFunctions : Type -> Bool
typeContainsFunctions ty =
    case ty of
        CA.TypeConstant _ ->
            False

        CA.TypeVariable { name } ->
            False

        CA.TypeFunction { from, fromIsMutable, to } ->
            True

        CA.TypeRecord args ->
            -- TODO is it ok to ignore the record extension?
            args.attrs
                |> Dict.values
                |> List.any typeContainsFunctions



----
--- Reorder definitions
--


reorderDefinitions : List (CA.ValueDefinition e) -> List (CA.ValueDefinition e)
reorderDefinitions defs =
    let
        names =
            defs
                |> List.map .name
                |> Set.fromList

        -- for each def, find all sibling defs it directly references
        referencedSiblingDefs : Dict Name (Set Name)
        referencedSiblingDefs =
            List.foldl (\def -> Dict.insert def.name (Set.intersect names (findAllRefs_definition def))) Dict.empty defs

        {- For each definition, find all the sibling it references AND the sibling /they/ reference (ie, recurse over the referenced sibs)

           This is kind of brute and horribly inefficient, but for now will do.
        -}
        allNestedSiblingRefs : Dict Name (Set Name)
        allNestedSiblingRefs =
            Dict.map (\k v -> findAllNestedSiblingReferences referencedSiblingDefs k Set.empty) referencedSiblingDefs

        oneReferencesTwo : Name -> Name -> Bool
        oneReferencesTwo one two =
            case Dict.get one allNestedSiblingRefs of
                Nothing ->
                    -- should not happen
                    False

                Just set ->
                    Set.member two set

        orderDefinitions : CA.ValueDefinition e -> CA.ValueDefinition e -> Order
        orderDefinitions a b =
            case ( oneReferencesTwo a.name b.name, oneReferencesTwo b.name a.name ) of
                ( True, True ) ->
                    -- Mutually recursive, order doesn't matter
                    EQ

                ( True, False ) ->
                    -- A should go after B
                    GT

                ( False, True ) ->
                    -- A should go before B
                    LT

                ( False, False ) ->
                    -- Neither references the other, order doesn't matter
                    EQ
    in
    List.sortWith orderDefinitions defs



-- stuff remainingDefs mutualGroups =
--   case remainingDefs of
--     [] ->
--       mutualGroups
--
--     (name, referecendSiblings) :: tail ->
--


findAllNestedSiblingReferences : Dict Name (Set Name) -> Name -> Set Name -> Set Name
findAllNestedSiblingReferences referencedSiblingDefs name accum =
    if Set.member name accum then
        accum

    else
        Set.foldl
            (findAllNestedSiblingReferences referencedSiblingDefs)
            (Set.insert name accum)
            (Dict.get name referencedSiblingDefs |> Maybe.withDefault Set.empty)



{-
   findMutualRecursions : Dict Name (Set Name) -> Name -> Set Name
   findMutualRecursions referencedSiblingDefs name =
       case Dict.get name referencedSiblingDefs of
           Nothing ->
               -- This is not supposed to happen
               found

           Just siblings ->
               let

                   testSibling sibName sibRefs alreadyChecked =
                     Set.insert sibName alreadyChecked
                     if Set.member name sibRefs then

                   notAlreadyChecked =
                       Set.diff siblings found
               in
               Set.foldl (findMutualRecursions referencedSiblingDefs) (Set.insert name found) notAlreadyChecked





   testRec defName path alreadyTried sibName mutualSet

     if sibName in alreadyTried
       mutualSet
     else
       let
       refs = Dict.get sibName referencedSiblingDefs

       newMutualSet =
         if defName in refs
           add all path names to mutualSet
         else
           mutualSet

       newAlreadyTried =
           Set.insert sibName alreadyTried

       newPath = sibName :: path

       toTry = Set.diff refs alreadyTried


       for any toTry
         testRec

     in



     get sib references

     notAlreadyTried = Set.diff sibReferences alreadyTried




     if sib references contain defname, then we have mutual recursion

-}
----
--- Find References
--


findAllRefs_definition : CA.ValueDefinition e -> Set String
findAllRefs_definition def =
    List.foldl (\stat -> Set.union (findAllRefs_statement stat)) Set.empty def.body


findAllRefs_statement : CA.Statement e -> Set String
findAllRefs_statement stat =
    case stat of
        CA.Definition def ->
            findAllRefs_definition def

        CA.Evaluation expr ->
            findAllRefs_expr expr


findAllRefs_expr : CA.Expression e -> Set String
findAllRefs_expr expr =
    case expr of
        CA.NumberLiteral _ args ->
            Set.empty

        CA.Variable _ args ->
            Set.singleton args.path

        CA.Lambda _ { start, parameter, body } ->
            findAllRefs_statementBlock body

        CA.Record _ args ->
            Dict.foldl (\name value -> Set.union (findAllRefs_expr value)) Set.empty args.attrs

        CA.Call _ { reference, argument } ->
            Set.union
                (findAllRefs_expr reference)
                (findAllRefs_arg argument)

        CA.If _ { start, condition, true, false } ->
            findAllRefs_statementBlock condition
                |> Set.union (findAllRefs_statementBlock true)
                |> Set.union (findAllRefs_statementBlock false)


findAllRefs_arg : CA.Argument e -> Set String
findAllRefs_arg arg =
    case arg of
        CA.ArgumentMutable { path } ->
            Set.singleton path

        CA.ArgumentExpression expr ->
            findAllRefs_expr expr


findAllRefs_statementBlock : List (CA.Statement e) -> Set String
findAllRefs_statementBlock statements =
    List.foldl (\stat -> Set.union (findAllRefs_statement stat)) Set.empty statements



----
---
--


type Either a b
    = Left a
    | Right b


partition : (a -> Either b c) -> List a -> ( List b, List c )
partition f ls =
    let
        fold item ( left, right ) =
            case f item of
                Left l ->
                    ( l :: left, right )

                Right r ->
                    ( left, r :: right )
    in
    List.foldr fold ( [], [] ) ls
