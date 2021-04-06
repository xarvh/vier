module Compiler.TypeInference exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Generator as TyGen
import Html
import Human.CanonicalAst as HumanCA
import Lib exposing (result_do)
import RefHierarchy
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Type)
import Types.Error as Error exposing (ErrorArgs, Res, errorTodo)
import Types.Literal


type alias Name =
    String


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


todoPos : CA.Pos
todoPos =
    { n = "ti"
    , c = ""
    , s = -1
    , e = -1
    }



----
---
--


dict_get : String -> comparable -> Dict comparable value -> value
dict_get caller k d =
    case Dict.get k d of
        Nothing ->
            Debug.todo caller

        Just v ->
            v


posToVarType : CA.Pos -> Type
posToVarType pos =
    CA.TypeVariable pos (CA.posToUid pos)



----
--- Variable type generator
--


type alias TyGen a =
    TyGen.Generator Int a


type alias TR a =
    TyGen (Res a)


newName : TyGen Name
newName =
    TyGen.next ((+) 1) String.fromInt


newType : CA.Pos -> TyGen Type
newType pos =
    TyGen.map (CA.TypeVariable pos) newName


do_nr : TR a -> (a -> TR b) -> TR b
do_nr nra f =
    TyGen.do nra
        (\ra ->
            case ra of
                Ok a ->
                    f a

                Err e ->
                    TyGen.wrap (Err e)
        )


map_nr : (a -> b) -> TR a -> TR b
map_nr f tr =
    TyGen.map (Result.map f) tr


list_foldl_nr : (item -> accum -> TR accum) -> List item -> accum -> TR accum
list_foldl_nr f ls accum =
    case ls of
        [] ->
            TyGen.wrap <| Ok accum

        head :: tail ->
            do_nr (f head accum) <| \newAccum ->
            list_foldl_nr f tail newAccum


list_map_nr : (a -> TR b) -> List a -> TR (List b)
list_map_nr f ls =
    list_foldl_nr (\a acc -> map_nr (\b -> b :: acc) (f a)) ls []
        |> map_nr List.reverse


dict_fold_nr : (comparable -> item -> accum -> TR accum) -> Dict comparable item -> accum -> TR accum
dict_fold_nr f dict accum =
    list_foldl_nr (\( k, v ) -> f k v) (Dict.toList dict) accum



----
--- Modules
--


inspectModule : Env -> CA.AllDefs -> Res ( CA.AllDefs, Env, Substitutions )
inspectModule prelude mod =
    result_do (Lib.dict_foldRes (\k -> addConstructors) mod prelude) <| \env ->
    let
        asValue rootDef =
            case rootDef of
                CA.Value v ->
                    Just v

                _ ->
                    Nothing

        statements =
            mod
                |> Dict.values
                |> List.filterMap asValue
                |> List.map CA.Definition

        gen =
            do_nr (inspectBlock statements env Dict.empty) <| \( shouldBeNone, env1, subs ) ->
            ( mod
            , refineEnv subs env1
            , subs
            )
                |> Ok
                |> TyGen.wrap
    in
    gen
        |> TyGen.run 0
        |> Tuple.first


addConstructors : CA.RootDef -> Env -> Res Env
addConstructors rootDef env =
    case rootDef of
        CA.Union def ->
            Lib.dict_foldRes (addConstructor def) def.constructors env

        _ ->
            Ok env


addConstructor : CA.UnionDef -> String -> List CA.Type -> Env -> Res Env
addConstructor unionDef ctorName ctorArgs env =
    let
        args =
            List.map (\name -> CA.TypeVariable todoPos name) unionDef.args

        ctorType =
            List.foldr fold (CA.TypeConstant todoPos unionDef.name args) ctorArgs

        fold ty accum =
            CA.TypeFunction todoPos ty (Just False) accum
    in
    case validateType False ctorType of
        Just err ->
            errorTodo err

        Nothing ->
            env
                |> Dict.insert ctorName
                    { type_ = ctorType
                    , forall = Set.fromList unionDef.args
                    , mutable = Just False
                    }
                |> Ok



----
--- Substitutions
--


refineType : Substitutions -> Type -> Type
refineType subs ty =
    case ty of
        CA.TypeConstant pos ref args ->
            CA.TypeConstant pos ref (List.map (refineType subs) args)

        CA.TypeVariable _ name ->
            case Dict.get name subs of
                Just substitutionType ->
                    -- a substitution exists for the variable type v
                    refineType subs substitutionType

                Nothing ->
                    -- no substitution, return the type as-is
                    ty

        CA.TypeFunction pos from fromIsMutable to ->
            CA.TypeFunction pos
                (refineType subs from)
                fromIsMutable
                (refineType subs to)

        CA.TypeAlias pos path t ->
            CA.TypeAlias pos path (refineType subs t)

        CA.TypeRecord pos extensible attrs ->
            case extensible |> Maybe.andThen (\name -> Dict.get name subs) of
                Nothing ->
                    CA.TypeRecord pos extensible (Dict.map (\name -> refineType subs) attrs)

                Just (CA.TypeVariable _ n) ->
                    CA.TypeRecord pos (Just n) (Dict.map (\name -> refineType subs) attrs)

                Just (CA.TypeRecord _ ext2 attrs2) ->
                    -- TODO I'm not sure what I'm doing here
                    CA.TypeRecord pos
                        ext2
                        -- TODO Should I refine ar? args? none? merge the two?
                        (Dict.map (\name -> refineType subs) attrs2)

                Just what ->
                    Debug.todo "replacing record extension with non-var" (Debug.toString what)


typeVarsFromType : Type -> Set Name
typeVarsFromType ty =
    case ty of
        CA.TypeVariable _ name ->
            Set.singleton name

        CA.TypeFunction _ from fromIsMutable to ->
            Set.union (typeVarsFromType from) (typeVarsFromType to)

        CA.TypeConstant pos ref args ->
            List.foldl (\a -> Set.union (typeVarsFromType a)) Set.empty args

        CA.TypeAlias _ path t ->
            typeVarsFromType t

        CA.TypeRecord _ extensible attrs ->
            let
                init =
                    case extensible of
                        Nothing ->
                            Set.empty

                        Just name ->
                            Set.singleton name
            in
            Dict.foldl (\n t -> Set.union (typeVarsFromType t)) init attrs



----
--- Env
--


instantiateType : Type -> Set Name -> TyGen Type
instantiateType t tvars =
    let
        -- substitute each tvar with a newly generated tvar
        substituteTvar : Name -> TyGen Substitutions -> TyGen Substitutions
        substituteTvar tvar genSubs =
            TyGen.do (newType todoPos) <| \nt ->
            TyGen.do genSubs <| \subs ->
            Dict.insert tvar nt subs
                |> TyGen.wrap

        genAllSubs : TyGen Substitutions
        genAllSubs =
            Set.foldl substituteTvar (TyGen.wrap Dict.empty) tvars
    in
    TyGen.do genAllSubs <| \subs ->
    refineType subs t
        |> TyGen.wrap


envGet : CA.Pos -> Name -> Env -> TR Type
envGet pos v e =
    case Dict.get v e of
        Just { type_, forall, mutable } ->
            instantiateType type_ forall
                |> TyGen.map Ok

        Nothing ->
            errorUnboundVariable pos v
                |> TyGen.wrap


refineEnv : Substitutions -> Env -> Env
refineEnv s env =
    let
        refine_entry _ entry =
            { entry | type_ = refineType (Set.foldl Dict.remove s entry.forall) entry.type_ }
    in
    Dict.map refine_entry env


unwrapAlias : Maybe String -> Type -> ( Type, Maybe String )
unwrapAlias prevPath ty =
    case ty of
        CA.TypeAlias _ path t ->
            unwrapAlias (Just path) t

        _ ->
            ( ty, prevPath )


type alias ErrorContext =
    { why : String
    , pos : CA.Pos
    }


unify : ErrorContext -> Type -> Type -> Substitutions -> TR Substitutions
unify ctx at1 at2 s =
    let
        -- TODO use path1,2 in error messages
        ( t1, path1 ) =
            unwrapAlias Nothing at1

        ( t2, path2 ) =
            unwrapAlias Nothing at2

        t1_refined =
            refineType s t1

        t2_refined =
            refineType s t2

        cycle v t =
            Set.member v (typeVarsFromType t)
    in
    case ( t1_refined, t2_refined ) of
        ( CA.TypeConstant _ c1_ref c1_args, CA.TypeConstant _ c2_ref c2_args ) ->
            if c1_ref /= c2_ref then
                errorCannotUnify ctx s t1_refined t2_refined

            else
                let
                    rec a1 a2 subs =
                        case ( a1, a2 ) of
                            ( [], [] ) ->
                                TyGen.wrap <| Ok subs

                            ( head1 :: tail1, head2 :: tail2 ) ->
                                do_nr (unify ctx head1 head2 subs) <| rec tail1 tail2

                            _ ->
                                TyGen.wrap <| errorTodo <| "one of the two has wrong number of args: " ++ c1_ref ++ " and " ++ c2_ref
                in
                rec c1_args c2_args s

        ( CA.TypeVariable _ v1_name, CA.TypeVariable _ v2_name ) ->
            TyGen.wrap
                (if v1_name == v2_name then
                    Ok s

                 else
                    s
                        |> Dict.insert v1_name t2_refined
                        |> Ok
                )

        ( CA.TypeVariable _ v1_name, _ ) ->
            TyGen.wrap
                (if cycle v1_name t2 then
                    -- is this the correct behavior?
                    errorTodo "cycle!"

                 else
                    s
                        |> Dict.insert v1_name t2_refined
                        |> Ok
                )

        ( _, CA.TypeVariable _ v2 ) ->
            unify ctx t2_refined t1_refined s

        ( CA.TypeFunction _ a_from a_fromIsMutable a_to, CA.TypeFunction _ b_from b_fromIsMutable b_to ) ->
            let
                maybeClash =
                    Maybe.map2 (\aa bb -> aa /= bb) a_fromIsMutable b_fromIsMutable
            in
            if Maybe.withDefault False maybeClash then
                TyGen.wrap <| errorTodo <| "mutability clash: " ++ Debug.toString t1_refined ++ " and " ++ Debug.toString t2_refined

            else
                do_nr (unify ctx a_to b_to s) <| unify ctx a_from b_from

        ( CA.TypeRecord _ a_ext a_attrs, CA.TypeRecord _ b_ext b_attrs ) ->
            unifyRecords ctx ( a_ext, a_attrs ) ( b_ext, b_attrs ) s

        _ ->
            errorCannotUnify ctx s t1_refined t2_refined


type alias UnifyRecordsFold =
    { aOnly : Dict Name Type
    , bOnly : Dict Name Type
    , both : Dict Name ( Type, Type )
    }


unifyRecords : ErrorContext -> ( Maybe String, Dict String Type ) -> ( Maybe String, Dict String Type ) -> Substitutions -> TR Substitutions
unifyRecords ctx ( a_ext, a_attrs ) ( b_ext, b_attrs ) subs0 =
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
            Dict.merge onA onBoth onB a_attrs b_attrs initState

        unifyBothRec subs ls =
            case ls of
                [] ->
                    TyGen.wrap <| Ok subs

                ( name, ( aType, bType ) ) :: tail ->
                    do_nr (unify ctx aType bType subs) <| \subsN ->
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
        case ( a_ext, b_ext ) of
            ( Just aName, Nothing ) ->
                if aOnly /= Dict.empty then
                    "a has attributes that do not exist in b"
                        |> errorTodo
                        |> TyGen.wrap

                else
                    -- substitute a with b
                    Dict.insert aName (CA.TypeRecord todoPos b_ext b_attrs) subs1
                        |> Ok
                        |> TyGen.wrap

            ( Nothing, Just bName ) ->
                if bOnly /= Dict.empty then
                    "b has attributes that do not exist in a"
                        |> errorTodo
                        |> TyGen.wrap

                else
                    -- substitute b with a
                    Dict.insert bName (CA.TypeRecord todoPos a_ext a_attrs) subs1
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
                        |> errorTodo
                        |> TyGen.wrap

            ( Just aName, Just bName ) ->
                TyGen.do newName <| \new ->
                let
                    subTy =
                        CA.TypeRecord todoPos (Just new) (Dict.union bOnly a_attrs)
                in
                subs1
                    |> Dict.insert aName subTy
                    |> Dict.insert bName subTy
                    |> Ok
                    |> TyGen.wrap


literalToType : Types.Literal.Value -> Type
literalToType l =
    case l of
        Types.Literal.Number _ ->
            Core.numberType

        Types.Literal.Text _ ->
            Core.textType

        Types.Literal.Char _ ->
            Core.charType


generalize : Set Name -> Env -> Type -> Set Name
generalize names env ty =
    let
        tyvarsFromEnv : Set Name
        tyvarsFromEnv =
            Dict.foldl addEnvTvar Set.empty env

        addEnvTvar k schema acc =
            -- don't add the value's own tyvars!
            if Set.member k names then
                acc

            else
                Set.union (Set.diff (typeVarsFromType schema.type_) schema.forall) acc
    in
    Set.diff (typeVarsFromType ty) tyvarsFromEnv



----
--- Inspect
--


type alias Eas =
    ( Env, Substitutions )


andEnv : Env -> TR Substitutions -> TR Eas
andEnv env =
    map_nr (\subs -> ( env, subs ))


unifyWithAttrPath : ErrorContext -> List Name -> Type -> Type -> Substitutions -> TR Substitutions
unifyWithAttrPath ctx attrPath typeAtPathEnd valueType subs =
    case attrPath of
        [] ->
            unify ctx typeAtPathEnd valueType subs

        head :: tail ->
            TyGen.do newName <| \n1 ->
            TyGen.do (newType todoPos) <| \t1 ->
            let
                -- `rType` : { n1 | `head` : t1 }
                rType =
                    CA.TypeRecord todoPos (Just n1) (Dict.singleton head t1)
            in
            do_nr (unify ctx rType valueType subs) <| \subs1 ->
            unifyWithAttrPath ctx tail typeAtPathEnd t1 subs1


inspectExpr : CA.Expression -> Type -> Eas -> TR Eas
inspectExpr expr ty ( env, subs ) =
    case expr of
        CA.Literal pos l ->
            subs
                |> unify { why = "Literal", pos = pos } ty (literalToType l)
                |> andEnv env

        CA.Variable pos { name, attrPath } ->
            -- Every time I use a var with variable type, it should be instantiated,
            -- because each time it may by used against a different type.
            -- This is done automatically by `envGet`.
            do_nr (envGet pos name env) <| \nt ->
            let
                t =
                    refineType subs nt
            in
            subs
                |> unifyWithAttrPath { why = "variable", pos = pos } attrPath ty t
                |> andEnv env

        CA.Lambda pos parameter body ->
            TyGen.do (newType pos) <| \argTy ->
            do_nr (inspectPattern insertVariableFromLambda parameter argTy ( env, subs )) <| \( env1, subs1 ) ->
            do_nr (inspectBlock body env1 subs1) <| \( returnType, env2, subs2 ) ->
            let
                lambdaTy =
                    posToVarType pos

                fromIsMutable_res =
                    case parameter of
                        CA.PatternDiscard _ ->
                            Ok (Just False)

                        CA.PatternAny _ name ->
                            Ok (dict_get "SNH inspectExpr CA.Lambda" name env2).mutable

                        _ ->
                            errorTodo "unpacking mutable arguments is not supported =("
            in
            do_nr (TyGen.wrap fromIsMutable_res) <| \fromIsMutable ->
            do_nr (unify { why = "Lambda", pos = pos } ty (CA.TypeFunction pos argTy fromIsMutable lambdaTy) subs2) <| \subs3 ->
            subs3
                |> unify { why = "Lambda return", pos = pos } lambdaTy returnType
                |> andEnv env

        CA.Call pos reference argument ->
            TyGen.do (newType pos) <| \argumentTy ->
            do_nr (inspectArgument env argument argumentTy subs) <| \( env1, subs1 ) ->
            let
                fromIsMutable =
                    case argument of
                        CA.ArgumentMutable _ ->
                            True

                        CA.ArgumentExpression _ ->
                            False

                funTy =
                    CA.TypeFunction pos argumentTy (Just fromIsMutable) ty
            in
            inspectExpr reference (refineType subs1 funTy) ( refineEnv subs1 env1, subs1 )

        CA.Record pos extends attrs ->
            do_nr (inspectRecordAttributes inspectExpr attrs ( env, subs )) <| \( attrTypes, ( env1, subs1 ) ) ->
            do_nr (inspectMaybeExtensible env1 extends ty subs1) <| \( extensible, ( env2, subs2 ) ) ->
            let
                refinedAttrTypes =
                    -- first I need all new subs, only then it makes sense to apply them
                    Dict.map (\attrName attrType -> refineType subs2 attrType) attrTypes
            in
            subs2
                |> unify { why = "Record", pos = pos } ty (CA.TypeRecord pos extensible refinedAttrTypes)
                |> andEnv env2

        CA.If pos ar ->
            do_nr (inspectBlock ar.condition env subs) <| \( conditionType, env1, subs1 ) ->
            do_nr (unify { why = "if condition", pos = pos } conditionType Core.boolType subs1) <| \subs2 ->
            do_nr (inspectBlock ar.true env1 subs2) <| \( inferredTrue, _, subs3 ) ->
            do_nr (inspectBlock ar.false env1 subs3) <| \( inferredFalse, _, subs4 ) ->
            do_nr (unify { why = "true and false", pos = pos } inferredTrue inferredFalse subs4) <| \subs5 ->
            do_nr (unify { why = "if branch", pos = pos } (refineType subs5 inferredTrue) ty subs5) <| \subs6 ->
            ( refineEnv subs6 env1
            , subs6
            )
                |> Ok
                |> TyGen.wrap

        CA.Try pos value tries ->
            let
                rawPatternTy =
                    posToVarType pos
            in
            TyGen.do (newType pos) <| \blockType ->
            do_nr (inspectExpr value rawPatternTy ( env, subs )) <| \( env1, subs1 ) ->
            let
                refPatternTy =
                    refineType subs1 rawPatternTy

                env2 =
                    refineEnv subs1 env1
            in
            do_nr (list_foldl_nr (inspectPatternBlock env2) tries ( refPatternTy, blockType, subs1 )) <| \( _, _, subs2 ) ->
            do_nr (unify { why = "try", pos = pos } blockType ty subs2) <| \subs3 ->
            ( refineEnv subs3 env2
            , subs3
            )
                |> Ok
                |> TyGen.wrap


inspectPatternBlock : Env -> ( CA.Pattern, List CA.Statement ) -> ( Type, Type, Substitutions ) -> TR ( Type, Type, Substitutions )
inspectPatternBlock env ( pattern, block ) ( patternType, expectedBlockType, subs ) =
    do_nr (inspectPattern insertVariableFromLambda pattern patternType ( env, subs )) <| \( env1, subs1 ) ->
    do_nr (inspectBlock block env1 subs1) <| \( inferredBlockType, _, subs2 ) ->
    do_nr (unify { why = "pa block", pos = todoPos } expectedBlockType inferredBlockType subs2) <| \subs3 ->
    ( refineType subs3 patternType
    , refineType subs3 inferredBlockType
    , subs3
    )
        |> Ok
        |> TyGen.wrap


{-| TODO replace this function with dict\_fold\_nr and inline it
-}
inspectRecordAttributes : (a -> Type -> Eas -> TR Eas) -> Dict Name a -> Eas -> TR ( Dict Name Type, Eas )
inspectRecordAttributes inspectValue attrs eas =
    let
        init =
            ( Dict.empty, eas )
                |> Ok
                |> TyGen.wrap

        foldAttr :
            Name
            -> a
            -> TR ( Dict Name Type, Eas )
            -> TR ( Dict Name Type, Eas )
        foldAttr attrName attrValue genResAccum =
            do_nr genResAccum <| \( attrsAccum, easAccum ) ->
            TyGen.do (newType todoPos) <| \nt ->
            do_nr (inspectValue attrValue nt easAccum) <| \newEas ->
            ( Dict.insert attrName nt attrsAccum, newEas )
                |> Ok
                |> TyGen.wrap
    in
    Dict.foldr foldAttr init attrs


inspectMaybeExtensible : Env -> Maybe CA.VariableArgs -> Type -> Substitutions -> TR ( Maybe Name, Eas )
inspectMaybeExtensible env maybeUpdateTarget ty subs =
    case maybeUpdateTarget of
        Nothing ->
            TyGen.wrap <| Ok <| ( Nothing, ( env, subs ) )

        Just updateTarget ->
            TyGen.do newName <| \n ->
            ( env, subs )
                |> inspectExpr (CA.Variable todoPos updateTarget) ty
                |> map_nr (\eas -> ( Just n, eas ))


inspectArgument : Env -> CA.Argument -> Type -> Substitutions -> TR Eas
inspectArgument env arg ty subs =
    case arg of
        CA.ArgumentMutable { name, attrPath } ->
            case Dict.get name env of
                Nothing ->
                    ("undeclared mutable variable: " ++ name)
                        |> errorTodo
                        |> TyGen.wrap

                Just schema ->
                    case schema.mutable of
                        Nothing ->
                            unifyWithAttrPath { why = "ArgumentMutable", pos = todoPos } attrPath ty schema.type_ subs
                                |> map_nr (\s -> ( Dict.insert name { schema | mutable = Just True } env, s ))

                        Just True ->
                            unifyWithAttrPath { why = "ArgumentMutable", pos = todoPos } attrPath ty schema.type_ subs
                                |> map_nr (\s -> ( env, s ))

                        Just False ->
                            (name ++ " can't be mutable")
                                |> errorTodo
                                |> TyGen.wrap

        CA.ArgumentExpression expr ->
            inspectExpr expr ty ( env, subs )



----
---
--


inspectStatement : CA.Statement -> Env -> Substitutions -> TR ( Type, Env, Substitutions )
inspectStatement statement env subs =
    case statement of
        CA.Evaluation expr ->
            TyGen.do (newType todoPos) <| \nt ->
            do_nr (inspectExpr expr nt ( env, subs )) <| \( env1, subs1 ) ->
            let
                refinedNt =
                    refineType subs1 nt
            in
            ( refinedNt, env1, subs1 )
                |> Ok
                |> TyGen.wrap

        CA.Definition { pattern, mutable, body, maybeAnnotation } ->
            let
                insert =
                    insertVariableFromDefinition mutable maybeAnnotation
            in
            do_nr (inspectBlock body env subs) <| \( bodyType, _, subs1 ) ->
            do_nr (inspectPattern insert pattern bodyType ( env, subs1 )) <| \( env1, subs2 ) ->
            let
                -- TODO All this stuff is just repeating stuff that insertVariableFromDefinition has done already.
                -- Can we avoid the duplication?
                names =
                    CA.patternNames pattern

                -- TODO we need to calculate forall only if there is an annotation
                refinedType =
                    refineType subs2 bodyType

                -- https://cstheory.stackexchange.com/questions/42554/extending-hindley-milner-to-type-mutable-references
                -- This is also the reason why we can't infer whether a value is mutable or not
                forall =
                    if mutable then
                        Set.empty

                    else
                        generalize names (refineEnv subs2 env) refinedType
            in
            case Maybe.map (\ann -> annotationTooGeneral ann forall) maybeAnnotation of
                Just (Just error) ->
                    errorTodo error
                        |> TyGen.wrap

                _ ->
                    -- The type of a definition is always None
                    Ok ( Core.noneType, refineEnv subs2 env1, subs2 )
                        |> TyGen.wrap


insertVariableFromDefinition : Bool -> Maybe Type -> Name -> Type -> Eas -> TR Eas
insertVariableFromDefinition mutable maybeAnnotation name ty ( env, subs ) =
    let
        def =
            dict_get "SNH inspectPattern PatternAny" name env
    in
    do_nr (unify { why = "insertVariableFromDefinition", pos = todoPos } ty def.type_ subs) <| \subs2 ->
    let
        refinedType =
            refineType subs2 def.type_

        -- https://cstheory.stackexchange.com/questions/42554/extending-hindley-milner-to-type-mutable-references
        -- This is also the reason why we can't infer whether a value is mutable or not
        forall =
            if mutable then
                Set.empty

            else
                generalize (Set.singleton name) (refineEnv subs2 env) refinedType

        scheme : EnvEntry
        scheme =
            { type_ = refinedType
            , forall = forall
            , mutable = Just mutable
            }

        env1 =
            Dict.insert name scheme env
    in
    ( env1, subs2 )
        |> Ok
        |> TyGen.wrap


insertVariableFromLambda : Name -> Type -> Eas -> TR Eas
insertVariableFromLambda name ty ( env, subs ) =
    if Dict.member name env then
        ("function parameter `" ++ name ++ "` shadows env variable")
            |> errorTodo
            |> TyGen.wrap

    else
        ( Dict.insert name { type_ = ty, forall = Set.empty, mutable = Nothing } env
        , subs
        )
            |> Ok
            |> TyGen.wrap


inspectPattern : (Name -> Type -> Eas -> TR Eas) -> CA.Pattern -> Type -> Eas -> TR Eas
inspectPattern insertVariable pattern ty ( env, subs ) =
    case pattern of
        CA.PatternDiscard _ ->
            ( env, subs )
                |> Ok
                |> TyGen.wrap

        CA.PatternAny _ name ->
            insertVariable name ty ( env, subs )

        CA.PatternLiteral _ literal ->
            subs
                |> unify { why = "pattern literal", pos = todoPos } ty (literalToType literal)
                |> andEnv env

        CA.PatternConstructor _ path args ->
            case Dict.get path env of
                Nothing ->
                    ("undeclared constructor: " ++ path)
                        |> errorTodo
                        |> TyGen.wrap

                Just envEntry ->
                    do_nr (reversedZipConstructorArgs args envEntry.type_ [] |> TyGen.wrap) <| \( patternType, argsAndTypes ) ->
                    do_nr (unify { why = "PatternConstructor", pos = todoPos } ty patternType subs) <| \subs1 ->
                    let
                        fold ( argPattern, argType ) eas =
                            inspectPattern insertVariable argPattern argType eas
                    in
                    list_foldl_nr fold argsAndTypes ( env, subs1 )

        CA.PatternRecord _ attrs ->
            TyGen.do newName <| \nn ->
            do_nr (dict_fold_nr (\name pa acc -> TyGen.map (\t -> Dict.insert name t acc |> Ok) (newType todoPos)) attrs Dict.empty) <| \xxx ->
            do_nr (unify { why = "PatternRecord", pos = todoPos } ty (CA.TypeRecord todoPos (Just nn) xxx) subs) <| \s1 ->
            let
                init =
                    ( Dict.empty, ( env, s1 ) )

                fold name pa ( attrsAcc, easAcc ) =
                    let
                        t =
                            dict_get "inspectPattern: CA.PatternRecord" name xxx
                    in
                    do_nr (inspectPattern insertVariable pa t easAcc) <| \newEasAcc ->
                    ( Dict.insert name t attrsAcc, newEasAcc )
                        |> Ok
                        |> TyGen.wrap
            in
            do_nr (dict_fold_nr fold attrs init) <| \( attrTypes, ( env1, subs1 ) ) ->
            let
                refinedAttrTypes =
                    -- first I need all new subs, only then it makes sense to apply them
                    Dict.map (\attrName attrType -> refineType subs1 attrType) attrTypes
            in
            subs1
                |> unify { why = "PatternRecord 2", pos = todoPos } ty (CA.TypeRecord todoPos (Just nn) refinedAttrTypes)
                |> andEnv env1


reversedZipConstructorArgs : List CA.Pattern -> Type -> List ( CA.Pattern, Type ) -> Res ( Type, List ( CA.Pattern, Type ) )
reversedZipConstructorArgs args constructorType accum =
    case constructorType of
        -- fromIsMutable should always be False for constructors
        CA.TypeFunction _ from fromIsMutable to ->
            case args of
                [] ->
                    errorTodo "not enough arguments in constructor pattern"

                a :: aTail ->
                    reversedZipConstructorArgs aTail to (( a, from ) :: accum)

        CA.TypeAlias _ _ ty ->
            reversedZipConstructorArgs args ty accum

        _ ->
            case args of
                [] ->
                    Ok ( constructorType, accum )

                a :: aTail ->
                    errorTodo "too many arguments in constructor pattern"


annotationTooGeneral : Type -> Set Name -> Maybe String
annotationTooGeneral annotation inferredForall =
    let
        -- This is already calculated when we add the raw definitions to env
        -- Is it faster to get it from env?
        annotationForall =
            typeVarsFromType annotation
    in
    if Set.size annotationForall > Set.size inferredForall then
        Just <| "annotation too general : " ++ Debug.toString annotationForall ++ " vs " ++ Debug.toString inferredForall

    else
        Nothing



----
--- Definition Body
--


inspectBlock : List CA.Statement -> Env -> Substitutions -> TR ( Type, Env, Substitutions )
inspectBlock stats parentEnv subs =
    if stats == [] then
        TyGen.do (newType todoPos) <| \nt ->
        ( nt, parentEnv, subs )
            |> Ok
            |> TyGen.wrap

    else
        let
            ( definitions, newStats ) =
                reorderStatements stats
        in
        do_nr (list_foldl_nr insertDefinitionRec definitions parentEnv) <| \localEnv ->
        do_nr (inspectStatementRec newStats Core.noneType localEnv subs) <| \typeAndEnvAndSubs ->
        TyGen.wrap <|
            let
                ( ty, env, _ ) =
                    typeAndEnvAndSubs

                defContainsFunctions : CA.ValueDef -> Bool
                defContainsFunctions def =
                    def.pattern
                        |> CA.patternNames
                        |> Set.toList
                        |> List.any nameContainsFunction

                nameContainsFunction name =
                    typeContainsFunctions (dict_get "SNH: nameContainsFunction" name env).type_

                definedMutables =
                    List.filter .mutable definitions

                mutablesWithFunction =
                    List.filter defContainsFunctions definedMutables
            in
            if mutablesWithFunction /= [] then
                mutablesWithFunction
                    |> List.foldl (.pattern >> CA.patternNames >> Set.union) Set.empty
                    |> Set.toList
                    |> List.sort
                    |> String.join ", "
                    |> (++) "these mutable values contain functions: "
                    |> errorTodo

            else if definedMutables /= [] && typeContainsFunctions ty then
                errorTodo "statement blocks that define mutables can't return functions"

            else
                Ok typeAndEnvAndSubs


inspectStatementRec : List CA.Statement -> Type -> Env -> Substitutions -> TR ( Type, Env, Substitutions )
inspectStatementRec stats returnType env subs =
    case stats of
        [] ->
            ( returnType, env, subs )
                |> Ok
                |> TyGen.wrap

        stat :: statsTail ->
            do_nr (inspectStatement stat env subs) <| \( ty, env1, subs1 ) ->
            inspectStatementRec statsTail ty env1 subs1


insertDefinitionRec : CA.ValueDef -> Env -> TR Env
insertDefinitionRec def env =
    let
        varNames =
            CA.patternNames def.pattern

        duplicates =
            Set.filter (\name -> Dict.member name env) varNames
    in
    if duplicates /= Set.empty then
        duplicates
            |> Set.toList
            |> List.sort
            |> String.join ", "
            |> (\s -> s ++ " already declared in scope!")
            |> errorTodo
            |> TyGen.wrap

    else
        case def.maybeAnnotation of
            Just annotation ->
                case validateType def.mutable annotation of
                    Just err ->
                        err
                            |> errorTodo
                            |> TyGen.wrap

                    Nothing ->
                        let
                            insert varName =
                                Dict.insert varName
                                    { type_ = annotation

                                    -- TODO remove parent annotation tyvars!
                                    , forall = typeVarsFromType annotation
                                    , mutable = Just def.mutable
                                    }
                        in
                        varNames
                            |> Set.foldl insert env
                            |> Ok
                            |> TyGen.wrap

            Nothing ->
                let
                    insert_nr varName e =
                        TyGen.do newName <| \typeName ->
                        e
                            |> Dict.insert varName
                                { type_ = CA.TypeVariable todoPos typeName
                                , forall = Set.singleton typeName
                                , mutable = Just def.mutable
                                }
                            |> Ok
                            |> TyGen.wrap
                in
                list_foldl_nr insert_nr (Set.toList varNames) env


statementAsDefinition : CA.Statement -> Maybe CA.ValueDef
statementAsDefinition stat =
    case stat of
        CA.Definition d ->
            Just d

        _ ->
            Nothing


validateType : Bool -> Type -> Maybe String
validateType mutable ty =
    case ty of
        CA.TypeConstant _ _ _ ->
            Nothing

        CA.TypeVariable _ name ->
            {- TODO
                if mutable then
                    Just "variable types can't be mutable"

               ----> except they can, they must be if we want to have functions
               capable of manipulating mutable containers

            -}
            Nothing

        CA.TypeAlias _ path t ->
            validateType mutable t

        CA.TypeFunction _ from fromIsMutable to ->
            if mutable then
                Just "mutable values can't contain functions"

            else
                case validateType (Maybe.withDefault False fromIsMutable) from of
                    Just e ->
                        Just e

                    Nothing ->
                        validateType False to

        CA.TypeRecord _ ext attrs ->
            attrs
                -- TODO Rewrite the whole dumpster fire to support returning  multiple errors
                |> Dict.values
                |> List.filterMap (validateType mutable)
                |> List.head


typeContainsFunctions : Type -> Bool
typeContainsFunctions ty =
    case ty of
        CA.TypeConstant _ _ _ ->
            False

        CA.TypeVariable _ name ->
            False

        CA.TypeFunction _ from fromIsMutable to ->
            True

        CA.TypeAlias _ path t ->
            typeContainsFunctions t

        CA.TypeRecord _ extensible attrs ->
            -- TODO is it ok to ignore the record extension?
            attrs
                |> Dict.values
                |> List.any typeContainsFunctions



----
--- Find References
--


findAllRefs_definition : CA.ValueDef -> Set String
findAllRefs_definition def =
    List.foldl (\stat -> Set.union (findAllRefs_statement stat)) Set.empty def.body


findAllRefs_statement : CA.Statement -> Set String
findAllRefs_statement stat =
    case stat of
        CA.Definition def ->
            findAllRefs_definition def

        CA.Evaluation expr ->
            findAllRefs_expr expr


{-| .

TODO move this to CA?
It is used also by CanonicalToJs

Also, maybe turn it into a CA.Expression -> Dict String VariableArgs

-}
findAllRefs_expr : CA.Expression -> Set String
findAllRefs_expr expr =
    case expr of
        CA.Literal _ args ->
            Set.empty

        CA.Variable _ args ->
            Set.singleton args.name

        CA.Lambda _ parameter body ->
            findAllRefs_statementBlock body

        CA.Record _ extends attrs ->
            -- TODO don't I need to add extends too?
            Dict.foldl (\name value -> Set.union (findAllRefs_expr value)) Set.empty attrs

        CA.Call _ reference argument ->
            Set.union
                (findAllRefs_expr reference)
                (findAllRefs_arg argument)

        CA.If _ { condition, true, false } ->
            findAllRefs_statementBlock condition
                |> Set.union (findAllRefs_statementBlock true)
                |> Set.union (findAllRefs_statementBlock false)

        CA.Try _ value patterns ->
            findAllRefs_expr value
                |> (\refs -> List.foldl (\( pa, block ) -> Set.union (findAllRefs_statementBlock block)) refs patterns)


findAllRefs_arg : CA.Argument -> Set String
findAllRefs_arg arg =
    case arg of
        CA.ArgumentMutable ar ->
            Set.singleton ar.name

        CA.ArgumentExpression expr ->
            findAllRefs_expr expr


findAllRefs_statementBlock : List CA.Statement -> Set String
findAllRefs_statementBlock statements =
    List.foldl (\stat -> Set.union (findAllRefs_statement stat)) Set.empty statements


{-| TODO move this outr of this module
-}
reorderStatements : List CA.Statement -> ( List CA.ValueDef, List CA.Statement )
reorderStatements stats =
    let
        definitionOrStatement stat =
            case stat of
                CA.Definition d ->
                    Lib.Left d

                _ ->
                    Lib.Right stat

        -- A statement list can contain definitions, creating its own scope
        -- Definitions can be recursive and in general appear in any order, so we want to add them to the environment before we inspect them
        ( definitions, nonDefs ) =
            Lib.partition definitionOrStatement stats

        -- Via patterns, a single definition can define multiple names, so we need to reference them by index instead
        indexedDefs =
            definitions
                |> List.indexedMap Tuple.pair

        indexByName =
            List.foldl
                (\( index, def ) dict -> Set.foldl (\name -> Dict.insert name index) dict (CA.patternNames def.pattern))
                Dict.empty
                indexedDefs

        findAllIndexes : ( Int, CA.ValueDef ) -> Set Int
        findAllIndexes ( index, def ) =
            def
                |> findAllRefs_definition
                |> Set.toList
                |> List.filterMap (\s -> Dict.get s indexByName)
                |> Set.fromList

        -- Also, we need to reorder them, so that dependent sibling defs come after
        orderedDefinitions =
            RefHierarchy.reorder Tuple.first findAllIndexes indexedDefs

        newStats =
            List.map (Tuple.second >> CA.Definition) orderedDefinitions ++ nonDefs
    in
    ( definitions, newStats )



----
--- Errors
--


errorUnboundVariable : CA.Pos -> String -> Res a
errorUnboundVariable pos s =
    Error.makeRes
        pos.n
        [ Error.showLines pos.c 2 pos.s
        , Error.text <| "unbound variable: " ++ s
        ]


errorCannotUnify : ErrorContext -> Substitutions -> CA.Type -> CA.Type -> TR a
errorCannotUnify ctx subs a b =
    [ Error.text <| "Cannot unify:"
    , Error.text <| "* t1 = " ++ HumanCA.typeToString a
    , Error.text <| "* t2 = " ++ HumanCA.typeToString b
    , Error.text <| "why : " ++ ctx.why
    , Error.text <| "expr = " ++ String.slice ctx.pos.s ctx.pos.e ctx.pos.c
    , Error.showLines ctx.pos.c 2 ctx.pos.s
    , subs
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map (\( n, t ) -> n ++ " = " ++ HumanCA.typeToString t)
        |> String.join "\n"
        |> Error.codeBlock
    ]
        |> Error.makeRes ctx.pos.n
        |> TyGen.wrap
