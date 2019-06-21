module Main exposing (main)

import Board exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Cell exposing (..)
import Core exposing (..)
import Render
import Time



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element { init = init, view = Render.view, update = update, subscriptions = subscriptions }



-- APP STATE


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { generation = 0
    , board = Board.generateFromPattern ( 10, 10 ) "Octagon2"
    , simSpeed = 0
    , viewSize = ( 800, 600 )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel, Cmd.none )

        Evolve ->
            let
                newBoard =
                    Board.evolve model.board
            in
            ( { model | generation = model.generation + 1, board = newBoard }, Cmd.none )

        SetSpeed speed ->
            ( { model | simSpeed = speed }, Cmd.none )

        CanvasClick pos ->
            let
                _ =
                    Debug.log "CanvasClick" pos
            in
            ( { model | board = Board.toggleCell pos model.board }, Cmd.none )



-- COMMANDS & TASKS


subscriptions : Model -> Sub Msg
subscriptions { simSpeed } =
    case simSpeed of
        0 ->
            Sub.none

        speed ->
            Time.every (1000 / toFloat simSpeed) (\_ -> Evolve)
