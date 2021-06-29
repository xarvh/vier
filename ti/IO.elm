module IO exposing (..)

import Dict exposing (Dict)


type IO state a
    = Wrapper (state -> ( a, state ))


do : IO s a -> (a -> IO s b) -> IO s b
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


return : a -> IO s a
return a =
    Wrapper (\state -> ( a, state ))


run : s -> IO s output -> ( output, s )
run s (Wrapper a) =
    a s



-- List


list_foldl : (item -> acc -> IO s acc) -> List item -> acc -> IO s acc
list_foldl f ls acc =
    case ls of
        [] ->
            return acc

        head :: tail ->
            do (f head acc) <| list_foldl f tail


list_map : (a -> IO s b) -> List a -> IO s (List b)
list_map f ls =
    let
        f_io a acc =
            do (f a) <| \b ->
            return (b :: acc)
    in
    do (list_foldl f_io ls []) <| (List.reverse >> return)


list_indexMap : (Int -> a -> IO s b) -> List a -> IO s (List b)
list_indexMap f ls =
    let
        f_io item ( index, acc ) =
            do (f index item) <| \b ->
            return ( index + 1, b :: acc )
    in
    do (list_foldl f_io ls ( 0, [] )) <| (Tuple.second >> List.reverse >> return)


list_for : List a -> (a -> IO s ()) -> IO s ()
list_for ls f =
    case ls of
        [] ->
            return ()

        head :: tail ->
            do (f head) <| \() ->
            list_for tail f



-- Dict


dict_foldl : (comparable -> item -> acc -> IO s acc) -> Dict comparable item -> acc -> IO s acc
dict_foldl f dict =
    let
        f_io ( key, item ) =
            f key item
    in
    list_foldl f_io (Dict.toList dict)


dict_map : (comparable -> a -> IO s b) -> Dict comparable a -> IO s (Dict comparable b)
dict_map f dict =
    let
        f_io k a acc =
            do (f k a) <| \b ->
            return (Dict.insert k b acc)
    in
    dict_foldl f_io dict Dict.empty
