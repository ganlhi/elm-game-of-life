module Core exposing (Model, Msg(..), Viewport)

import Board exposing (Board)


type alias Viewport =
    { zoom : Float
    , topLeft : ( Float, Float )
    }


type alias Model =
    { generation : Int
    , board : Board
    , simSpeed : Int
    , viewSize : ( Int, Int )
    , viewport : Viewport
    , panning : Bool
    }


type Msg
    = Noop
    | Evolve
    | Reset
    | SetSpeed Int
    | ZoomIn
    | ZoomOut
    | ToggleCell ( Int, Int )
    | Panning Bool
    | Movement ( Float, Float )
