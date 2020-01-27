module Game exposing (Player, Status, TableSize, init, playerToString, putPiece)

import Array exposing (..)


type Player
    = Closs
    | Circle
    | None


type alias Tables =
    Array Player


type alias TableSize =
    { x : Int
    , y : Int
    }


type alias Status =
    { player : Player
    , result : Player
    , table : Tables
    , size : TableSize
    }


playerToString : Player -> String
playerToString player =
    case player of
        Closs ->
            "X"

        Circle ->
            "O"

        None ->
            "_"


init : Int -> Int -> Status
init x y =
    { player = Closs
    , result = None
    , table =
        Array.repeat (x * y) None
    , size = { x = x, y = y }
    }


putPiece : Int -> Int -> Status -> Result String Status
putPiece x y status =
    if isEnding status then
        Err "game is finish"

    else if isAllFull status.table then
        Err "all is full"

    else
        case isSuccessMove x y status of
            Ok index ->
                status.table
                    |> Array.set index status.player
                    |> checkIsFinish status.player
                    |> conbineStatus status
                    |> Ok

            Err msg ->
                Err msg


isAllFull : Tables -> Bool
isAllFull table =
    table
        |> Array.filter (\x -> x == None)
        |> Array.length
        |> (\len -> len == 0)


isSuccessMove : Int -> Int -> Status -> Result String Int
isSuccessMove x y status =
    let
        index =
            x * status.size.y + y

        table =
            status.table
    in
    case Array.get index table of
        Just val ->
            if val == None then
                Ok index

            else
                Err "there have other pieces"

        Nothing ->
            Err "coordinate is incorrect"


isEnding : Status -> Bool
isEnding status =
    status.result /= None


checkIsFinish : Player -> Tables -> Result Tables Tables
checkIsFinish player table =
    if isFinish player table then
        Ok table

    else
        Err table


isFinish : Player -> Tables -> Bool
isFinish player table =
    table
        |> Array.toIndexedList
        |> getEveryLine
            [ [ 0, 1, 2 ]
            , [ 3, 4, 5 ]
            , [ 6, 7, 8 ]
            , [ 0, 3, 6 ]
            , [ 1, 4, 7 ]
            , [ 2, 5, 8 ]
            , [ 0, 4, 8 ]
            , [ 2, 4, 6 ]
            ]
        |> List.map (isItemsEqual player)
        |> List.foldl (||) False


getEveryLine : List (List Int) -> List ( Int, Player ) -> List (List Player)
getEveryLine lines table =
    List.map (getLine table) lines


getLine : List ( Int, Player ) -> List Int -> List Player
getLine table line =
    line
        |> List.concatMap
            (\index ->
                List.filter (\value -> Tuple.first value == index) table
            )
        |> List.map Tuple.second


isItemsEqual : a -> List a -> Bool
isItemsEqual val arr =
    List.foldl (\cur res -> res && cur == val) True arr


conbineStatus : Status -> Result Tables Tables -> Status
conbineStatus status maybeFinish =
    let
        nextPlayer =
            if status.player == Closs then
                Circle

            else
                Closs
    in
    case maybeFinish of
        Ok newTable ->
            { status
                | result = status.player
                , table = newTable
            }

        Err newTable ->
            { status
                | player = nextPlayer
                , table = newTable
            }
