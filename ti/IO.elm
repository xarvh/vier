module IO exposing (..)

import Dict exposing (Dict)


type IO a
    = Wrapper (State -> ( a, State ))


type alias State =
    { nextId : Int
    }



--


newId : IO Int
newId =
    Wrapper (\state -> ( state.nextId, { state | nextId = state.nextId + 1 } ))



--


do : IO a -> (a -> IO b) -> IO b
do (Wrapper genA) f =
    Wrapper
        (\state ->
            let
                ( result, newState ) =
                    genA state

                (Wrapper genB) =
                    f result
            in
            genB newState
        )


return : a -> IO a
return a =
    Wrapper (\state -> ( a, state ))


run : State -> IO output -> ( output, State )
run s (Wrapper a) =
    a s



-- List


list_foldl : (item -> acc -> IO acc) -> List item -> acc -> IO acc
list_foldl f ls acc =
    case ls of
        [] ->
            return acc

        head :: tail ->
            do (f head acc) <| list_foldl f tail


list_map : (a -> IO b) -> List a -> IO (List b)
list_map f ls =
    let
        f_io a acc =
            do (f a) <| \b ->
            return (b :: acc)
    in
    do (list_foldl f_io ls []) <| (List.reverse >> return)


list_indexMap : (Int -> a -> IO b) -> List a -> IO (List b)
list_indexMap f ls =
    let
        f_io item ( index, acc ) =
            do (f index item) <| \b ->
            return ( index + 1, b :: acc )
    in
    do (list_foldl f_io ls ( 0, [] )) <| (Tuple.second >> List.reverse >> return)



-- Dict


dict_foldl : (comparable -> item -> acc -> IO acc) -> Dict comparable item -> acc -> IO acc
dict_foldl f dict =
    let
        f_io ( key, item ) =
            f key item
    in
    list_foldl f_io (Dict.toList dict)


dict_map : (comparable -> a -> IO b) -> Dict comparable a -> IO (Dict comparable b)
dict_map f dict =
    let
        f_io k a acc =
            do (f k a) <| \b ->
            return (Dict.insert k b acc)
    in
    dict_foldl f_io dict Dict.empty
