module Type exposing (..)

import Array exposing (Array)
import CanonicalAst as CA exposing (Name, Pos)
import Dict exposing (Dict)
import IO



-- placeholer types


type alias Name =
    String


{-| This is a reference to an allocated Descriptor
-}
type Variable
    = -- TODO: UF.Point Descriptor
      Variable Int


{-| This is a reference to an AllocatedPools
-}
type Pools
    = Pools Int



-- actual types


{-| This models the "type" used by the inference algorithm

         = PlaceHolder Name.Name
         | AliasN ModuleName.Canonical Name.Name [(Name.Name, Type)] Type
         | UnitN
         | TupleN Type Type (Maybe Type)

-}
type Type
    = VarN CA.TyVarRef
    | AppN CA.ModuleName Name (List Type)
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Dict Name Type) Type



-- Descriptor


type alias Descriptor =
    { content : Content
    , rank : Int
    , mark : Mark
    , copy : Maybe Variable
    }


type Content
    = FlexVar (Maybe Name)



{-
   | FlexSuper SuperType (Maybe Name.Name)
   | RigidVar Name.Name
   | RigidSuper SuperType Name.Name
   | Structure FlatType
   | Alias ModuleName.Canonical Name.Name [(Name.Name,Variable)] Variable
   | Error
-}
{-
   data SuperType
     = Number
     | Comparable
     | Appendable
     | CompAppend
     deriving (Eq)
-}


initDescriptor : Content -> Descriptor
initDescriptor content =
    { content = content
    , rank = noRank
    , mark = noMark
    , copy = Nothing
    }



-- Pools


type alias AllocatedPools =
    Array (List Variable)


poolsInit : AllocatedPools
poolsInit =
    Array.repeat 8 []


poolsNew : AllocatedPools -> IO Pools
poolsNew initValue =
    -- "MVector.replicate 8 []"
    let
        nextId : State -> Int
        nextId state =
            state.allocatedPools
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        allocate : State -> ( Variable, State )
        allocate state =
            let
                id =
                    nextId state
            in
            ( Variable id
            , { state | allocatedPools = Dict.insert id initValue state.allocatedPools }
            )
    in
    Wrapper allocate


poolsModify : Pools -> (List Variable -> List Variable) -> Int -> IO ()
poolsModify (Pools id) f index =
    -- "MVector.modify pools"
    let
        upd : Maybe (Array (List Variable)) -> Maybe (Array (List Variable))
        upd maybeAllocatedPools =
            case maybeAllocatedPools of
                Nothing ->
                    Debug.todo <| "segmentation fault! XD accessing pool " ++ String.fromInt id

                Just allocatedPools ->
                    case Array.get index allocatedPools of
                        Nothing ->
                            Debug.todo <| "segmentation fault! XD accessing pool " ++ String.fromInt id ++ " index: " ++ String.fromInt index

                        Just pool ->
                            Array.set index (f pool) allocatedPools
                                |> Just

        modify : State -> ( (), State )
        modify state =
            ( ()
            , { state | allocatedPools = Dict.update id upd state.allocatedPools }
            )
    in
    IO.Wrapper modify


poolsLength : Pools -> IO Int
poolsLength (Pools id) =
    let
        length : State -> ( Int, State )
        length state =
            case Dict.get id state.allocatedPools of
                Nothing ->
                    Debug.todo "poolsLength"

                Just pools ->
                    ( Array.size pools, state )
    in
    IO.Wrapper length


poolsWrite : Pools -> Int -> List Variable -> IO ()
poolsWrite (Pools id) index vars =
    -- MVector.write nextPools nextRank vars
    let
        upd : Maybe (Array (List Variable)) -> Maybe (Array (List Variable))
        upd maybeAllocatedPools =
            case maybeAllocatedPools of
                Nothing ->
                    Debug.todo <| "segmentation fault! XD accessing pool " ++ String.fromInt id

                Just allocatedPools ->
                    Array.set index vars allocatedPools
                        |> Just

        write : State -> ( (), State )
        write state =
            ( ()
            , { state | allocatedPools = Dict.update id upd state.allocatedPools }
            )
    in
    IO.Wrapper write


poolsGrow : Pools -> Int -> IO Pools
poolsGrow (Pools id) numberOfAdditionalElements =
    -- MVector.grow pools poolsLength
    {- https://hackage.haskell.org/package/vector-0.12.3.0/docs/Data-Vector-Mutable.html#g:8

       > [..] only pointers to values are copied over, therefore values themselves will be shared between two vectors.
       > This is an important distinction to know about [..] in case when values themselves are of a mutable type, eg. IORef or another mutable vector.

       Our values are Lists of IORefs.
    -}
    let
        read : State -> ( AllocatedPools, State )
        read state =
            case Dict.get id state.allocatedPools of
                Nothing ->
                    Debug.todo <| "grow: segmentation fault! XD accessing pool " ++ String.fromInt id

                Just allocatedPools ->
                    ( allocatedPools, state )
    in
    IO.do (IO.Wrapper read) <| \allocatedPools ->
    Array.repeat numberOfAdditionalElements []
        |> Array.append allocatedPools
        |> poolsNew



--
-- IO Monad
--


type alias IO a =
    IO.IO State a


type alias State =
    { allocatedDescriptors : Dict Int Descriptor
    , allocatedPools : Dict Int Pools
    }


allocateDescriptor : Descriptor -> IO Variable
allocateDescriptor descriptor =
    let
        nextId : State -> Int
        nextId state =
            state.allocatedDescriptors
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        allocate : State -> ( Variable, State )
        allocate state =
            let
                id =
                    nextId state
            in
            ( Variable id
            , { state | allocatedDescriptors = Dict.insert id descriptor state.allocatedDescriptors }
            )
    in
    Wrapper allocate



-- Flex vars


mkFlexVar : IO Variable
mkFlexVar =
    unnamedFlexVar
        |> initDescriptor
        |> allocateVariable


nameToFlex : Name -> IO Variable
nameToFlex name =
    name
        |> Just
        |> FlexVar
        |> initDescriptor
        |> allocateVariable


unnamedFlexVar : Content
unnamedFlexVar =
    FlexVar Nothing



-- Ranks


noRank : Int
noRank =
    0



-- Marks


type Mark
    = Mark Int


noMark : Mark
noMark =
    Mark 2


occursMark : Mark
occursMark =
    Mark 1


getVarNamesMark : Mark
getVarNamesMark =
    Mark 0
