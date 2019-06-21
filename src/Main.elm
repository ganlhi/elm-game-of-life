module Main exposing (main)

import Board exposing (..)
import Browser
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
    , board = Board.generateFromPattern "Octagon2"
    , simSpeed = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel, Cmd.none )

        Evolve ->
            ( { model | generation = model.generation + 1, board = Board.evolve model.board }, Cmd.none )



-- COMMANDS & TASKS


subscriptions : Model -> Sub Msg
subscriptions { simSpeed } =
    case simSpeed of
        0 ->
            Sub.none

        speed ->
            Time.every (1000 / toFloat simSpeed) (\_ -> Evolve)
