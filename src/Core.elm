module Core exposing (Model, Msg(..))

import Board exposing (Board)


type alias Model =
    { generation : Int, board : Board, simSpeed : Int, viewSize : ( Int, Int ) }


type Msg
    = Evolve
    | Reset
    | SetSpeed Int
    | CanvasClick ( Int, Int )
