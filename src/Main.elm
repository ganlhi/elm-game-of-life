module Main exposing (main)

import Board exposing (Board)
import Browser
import Core exposing (Model, Msg(..), Viewport)
import Patterns
import Randomize exposing (randomizeByDensity)
import Render
import Time



-- PROGRAM


main : Program ( Int, Int ) Model Msg
main =
    Browser.element { init = init, view = Render.view, update = update, subscriptions = subscriptions }



-- APP STATE


init : ( Int, Int ) -> ( Model, Cmd Msg )
init viewSize =
    ( initialModel viewSize, Cmd.none )


initialModel : ( Int, Int ) -> Model
initialModel viewSize =
    { generation = 0
    , board = initialBoard
    , simSpeed = 0
    , viewSize = viewSize
    , viewport = { zoom = 10, topLeft = getViewportOffset viewSize }
    , panning = False
    }


getViewportOffset : ( Int, Int ) -> ( Float, Float )
getViewportOffset ( width, height ) =
    ( toFloat -width / 2, toFloat -height / 2 )


initialBoard : Board
initialBoard =
    Patterns.generateBoard ( 10, 10 ) "Octagon2"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reset ->
            ( { model
                | generation = 0
                , board = initialBoard
                , simSpeed = 0
              }
            , Cmd.none
            )

        Evolve ->
            let
                newBoard =
                    Board.evolve model.board
            in
            ( { model
                | generation = model.generation + 1
                , board = newBoard
              }
            , Cmd.none
            )

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

        Randomize density ->
            ( model, randomizeByDensity ( 50, 50 ) density )

        InsertRandomPattern ( width, height ) values ->
            let
                newBoard =
                    Board.generateFromList ( -width // 2, -height // 2 ) width values
            in
            ( { model | board = newBoard }, Cmd.none )


zoom : Float -> Viewport -> Viewport
zoom coeff vp =
    { vp | zoom = vp.zoom * coeff }


pan : ( Float, Float ) -> Viewport -> Viewport
pan ( dx, dy ) vp =
    { vp | topLeft = ( Tuple.first vp.topLeft - dx, Tuple.second vp.topLeft - dy ) }



-- COMMANDS & TASKS


subscriptions : Model -> Sub Msg
subscriptions { simSpeed } =
    let
        autorunSub =
            case simSpeed of
                0 ->
                    Sub.none

                speed ->
                    Time.every (1000 / toFloat speed) (\_ -> Evolve)
    in
    Sub.batch
        [ autorunSub
        ]
