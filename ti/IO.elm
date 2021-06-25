module IO exposing (..)


type IO a
    = Wrapper (State -> ( a, State ))


type alias State =
    Int


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


newId : IO Int
newId =
    Wrapper (\state -> ( state, state + 1 ))


indexedMap_list : (Int -> a -> IO b) -> List a -> IO (List b)
indexedMap_list f ls =
    do (indexedMap_listRec f 0 [] ls) <| (List.reverse >> return)


indexedMap_listRec : (Int -> a -> IO b) -> Int -> List b -> List a -> IO (List b)
indexedMap_listRec f n accum ls =
    case ls of
        [] ->
            return accum

        head :: tail ->
            do (f n head) <| \b ->
            indexedMap_listRec f (n + 1) (b :: accum) tail


run : State -> IO output -> ( output, State )
run s (Wrapper a) =
    a s
