module StringToTokens exposing (..)

import Array exposing (Array)


type ErrorType
    = ErrorTab
    | ErrorNewlineInsideSoftQuote
    | ErrorHardQuoteClosesSoftQuote


type alias Error =
    { position : Int
    , t : ErrorType
    }



----
--- Chunks
--


type ChunkType
    = ContentLine
    | SingleLineComment
    | MultiLineComment Int
    | SoftQuotedString
    | HardQuotedString


type alias Chunk =
    { t : ChunkType
    , start : Int
    , end : Int
    }


type alias State =
    { position : Int
    , chunks : List Chunk
    , chunkType : ChunkType
    , chunkStart : Int

    -- This doesn't change
    , code : Array Char
    }


stringToChunks : String -> Result Error (List Chunk)
stringToChunks codeAsString =
    { position = 0
    , chunks = []
    , chunkType = ContentLine
    , chunkStart = 0
    , code =
        codeAsString
            |> String.toList
            |> Array.fromList
    }
        |> stringToChunksRec
        |> Result.map (.chunks >> List.reverse)


finaliseCurrentChunk : ChunkType -> State -> State
finaliseCurrentChunk newChunkType state =
    { state
        | chunkStart = state.position
        , chunkType = newChunkType
        , chunks =
            if state.position == state.chunkStart then
                state.chunks

            else
                { t = state.chunkType
                , start = state.chunkStart
                , end = state.position
                }
                    :: state.chunks
    }


stringToChunksRec : State -> Result Error State
stringToChunksRec state =
    let
        q =
            Debug.log "" ( Array.length state.code, state.position )
    in
    if state.position >= Array.length state.code then
        -- ContentLine is not really used
        state
            |> finaliseCurrentChunk ContentLine
            |> Ok

    else
        let
            newLine =
                compare_consume_thenChangeChunkType "\n" ContentLine

            lineComment_Start =
                compare_changeChunkType_thenConsume "--" SingleLineComment

            lineComment_End =
                compare_consume_thenChangeChunkType "\n" ContentLine

            hardQuotedString_Start =
                compare_changeChunkType_thenConsume "\"\"\"" HardQuotedString

            hardQuotedString_End =
                compare_consume_thenChangeChunkType "\"\"\"" ContentLine

            softQuotedString_Start =
                compare_changeChunkType_thenConsume "\"" SoftQuotedString

            softQuotedString_End =
                compare_consume_thenChangeChunkType "\"" ContentLine

            multiComment_Start chunkType =
                compare_changeChunkType_thenConsume "{-"
                    (case chunkType of
                        MultiLineComment depth ->
                            MultiLineComment (depth + 1)

                        _ ->
                            MultiLineComment 0
                    )

            multiComment_End depth =
                compare_changeChunkType_thenConsume "-}"
                    (if depth == 0 then
                        ContentLine

                     else
                        MultiLineComment <| depth - 1
                    )

            tests =
                case state.chunkType of
                    ContentLine ->
                        [ errorOn "\t" ErrorTab
                        , multiComment_Start state.chunkType
                        , newLine
                        , lineComment_Start
                        , hardQuotedString_Start
                        , softQuotedString_Start
                        ]

                    SingleLineComment ->
                        [ lineComment_End ]

                    MultiLineComment depth ->
                        [ multiComment_Start state.chunkType
                        , multiComment_End depth
                        ]

                    SoftQuotedString ->
                        [ errorOn "\n" ErrorNewlineInsideSoftQuote
                        , errorOn "\"\"\"" ErrorHardQuoteClosesSoftQuote
                        , softQuotedString_End
                        ]

                    HardQuotedString ->
                        [ hardQuotedString_End ]
        in
        matchFirst state tests


matchFirst : State -> List (State -> Maybe (Result Error State)) -> Result Error State
matchFirst state testFunctions =
    case testFunctions of
        -- No special markers found, so it's the same chunk
        [] ->
            state
                |> consumeChars 1
                |> stringToChunksRec

        head :: tail ->
            case head state of
                Nothing ->
                    matchFirst state tail

                Just result ->
                    result
                        |> Result.andThen stringToChunksRec


consumeChars : Int -> State -> State
consumeChars length state =
    { state | position = state.position + length }


{-| Compare a string with a position in State

TODO I assume this is particularly slow

-}
compare : String -> State -> Bool
compare targetAsString state =
    let
        target =
            targetAsString
                |> String.toList
                |> Array.fromList

        compareRec offset =
            case Array.get offset target of
                Nothing ->
                    -- The two strings match!
                    True

                Just targetChar ->
                    case Array.get (state.position + offset) state.code of
                        Nothing ->
                            False

                        Just codeChar ->
                            if targetChar == codeChar then
                                compareRec (offset + 1)

                            else
                                False
    in
    compareRec 0


errorOn : String -> ErrorType -> State -> Maybe (Result Error State)
errorOn target errorType state =
    if compare target state then
        Just <| Err { t = errorType, position = state.position }

    else
        Nothing


compare_consume_thenChangeChunkType : String -> ChunkType -> State -> Maybe (Result Error State)
compare_consume_thenChangeChunkType target chunkType state =
    if compare target state then
        state
            |> consumeChars (String.length target)
            |> finaliseCurrentChunk chunkType
            |> Ok
            |> Just

    else
        Nothing


compare_changeChunkType_thenConsume : String -> ChunkType -> State -> Maybe (Result Error State)
compare_changeChunkType_thenConsume target chunkType state =
    if compare target state then
        state
            |> finaliseCurrentChunk chunkType
            |> consumeChars (String.length target)
            |> Ok
            |> Just

    else
        Nothing
