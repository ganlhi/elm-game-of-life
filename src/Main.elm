module Main exposing (main)

import Board
import Browser
import Core exposing (Model, Msg(..), Viewport)
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
    , viewport = { zoom = 10, topLeft = ( 0, 0 ) }
    , panning = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

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

        ZoomIn ->
            ( { model | viewport = model.viewport |> zoom 1.1 }, Cmd.none )

        ZoomOut ->
            ( { model | viewport = model.viewport |> zoom 0.9 }, Cmd.none )

        ToggleCell pos ->
            ( { model | board = Board.toggleCell pos model.board }, Cmd.none )

        Movement mv ->
            ( { model | viewport = model.viewport |> pan mv }, Cmd.none )

        Panning panning ->
            ( { model | panning = panning }, Cmd.none )


zoom : Float -> Viewport -> Viewport
zoom coeff vp =
    { vp | zoom = vp.zoom * coeff }


pan : ( Float, Float ) -> Viewport -> Viewport
pan ( dx, dy ) vp =
    { vp | topLeft = ( Tuple.first vp.topLeft - dx, Tuple.second vp.topLeft - dy ) }



-- COMMANDS & TASKS


subscriptions : Model -> Sub Msg
subscriptions { simSpeed } =
    case simSpeed of
        0 ->
            Sub.none

        speed ->
            Time.every (1000 / toFloat speed) (\_ -> Evolve)
