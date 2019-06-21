module Core exposing (Model, Msg(..))

import Board exposing (Board)


type alias Model =
    { generation : Int, board : Board, simSpeed : Int }


type Msg
    = Evolve
    | Reset
