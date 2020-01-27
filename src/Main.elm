module Main exposing (main)

import Array exposing (..)
import Browser
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Step =
    { player : Game.Player
    , x : Int
    , y : Int
    }


type alias Model =
    { tipMessage : String
    , gameStatus : Game.Status
    , stepLogs : List Step
    }


type Msg
    = ClickCell Int Int
    | ToReStart


init : Model
init =
    Model "" (Game.init 3 3) []


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickCell x y ->
            updateStatus x y model

        ToReStart ->
            { model
                | tipMessage = "RESTART!"
                , gameStatus = Game.init 3 3
                , stepLogs = []
            }


view : Model -> Html Msg
view model =
    div [ class "frame" ]
        [ div [ class "gameBox" ] (gameBox model.gameStatus)
        , div [ class "gameStr", onClick ToReStart ] [ text "click here to re-start!" ]
        , div [ class "gameTip" ] [ text model.tipMessage ]
        , div [ class "gameSys" ] [ text (gameSystemMessage model.gameStatus) ]
        , div [ class "gameLog" ] (gameLog model.stepLogs)
        ]



-- functions
-- update


updateStatus : Int -> Int -> Model -> Model
updateStatus x y model =
    if model.tipMessage /= "game is finish" then
        model.gameStatus
            |> Game.putPiece x y
            |> getNewStatus x y model

    else
        model


getNewStatus : Int -> Int -> Model -> Result String Game.Status -> Model
getNewStatus x y modelOri modelRes =
    case modelRes of
        Ok newStatus ->
            { modelOri
                | tipMessage = ""
                , gameStatus = newStatus
                , stepLogs =
                    modelOri.stepLogs
                        ++ [ { player = modelOri.gameStatus.player
                             , x = x
                             , y = y
                             }
                           ]
            }

        Err newTip ->
            { modelOri
                | tipMessage = newTip
            }



-- view
-- gameBox


gameBox : Game.Status -> List (Html Msg)
gameBox gameStatus =
    let
        table =
            gameStatus.table

        size =
            gameStatus.size
    in
    table
        |> Array.toList
        |> List.indexedMap
            (\idx plr -> gameBoxCell (Game.playerToString plr) size idx)


gameBoxCell : String -> Game.TableSize -> Int -> Html Msg
gameBoxCell content size index =
    let
        x =
            index // size.y

        y =
            modBy size.y index
    in
    div
        [ class "cell", onClick (ClickCell x y) ]
        [ text content ]


gameSystemMessage : Game.Status -> String
gameSystemMessage gameStatus =
    let
        res =
            gameStatus.result

        player =
            gameStatus.player
    in
    case Game.playerToString res of
        "X" ->
            "winner is: X"

        "O" ->
            "winner is: O"

        _ ->
            "now player: " ++ Game.playerToString player



-- gameLog


gameLog : List Step -> List (Html Msg)
gameLog steps =
    List.map getStepHtml steps


getStepHtml : Step -> Html Msg
getStepHtml step =
    let
        player =
            Game.playerToString step.player

        x =
            String.fromInt step.x

        y =
            String.fromInt step.y
    in
    div [ class "step" ]
        [ text (player ++ ": " ++ x ++ ", " ++ y) ]
