module Core exposing (Model, Msg(..))

import Board exposing (Board)


type alias Model =
    { generation : Int, board : Board, simSpeed : Int, viewSize : ( Int, Int ), zoomFactor : Float }


type Msg
    = Evolve
    | Reset
    | SetSpeed Int
    | ZoomIn
    | ZoomOut
    | CanvasClick ( Int, Int )
