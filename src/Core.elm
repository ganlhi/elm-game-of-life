module Core exposing (Model, Msg(..))

import Board exposing (Board)


type alias Model =
    { generation : Int
    , board : Board
    , simSpeed : Int
    , viewSize : ( Int, Int )
    , viewTopLeft : ( Float, Float )
    , zoomFactor : Float
    , panning : Maybe ( Float, Float )
    }


type Msg
    = Noop
    | Evolve
    | Reset
    | SetSpeed Int
    | ZoomIn
    | ZoomOut
    | CanvasClick ( Int, Int )
    | Panning (Maybe ( Float, Float ))
