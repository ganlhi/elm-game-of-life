module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..), neighbours)
import Task
import Time



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- APP STATE


type alias Model =
    { generation : Int, params : Params, board : Board }


type Msg
    = Evolve
    | SetSpeed Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { generation = 0
    , board = makeStartingBoard
    , params =
        { autorunSpeed = 0
        , showPredictions = False
        }
    }


makeStartingBoard : Board
makeStartingBoard =
    -- as an example, generate a 10x10 board with an Octagon2 pulser in the middle
    makeBoard 10 10 [ ( 4, 1 ), ( 5, 1 ), ( 3, 2 ), ( 6, 2 ), ( 2, 3 ), ( 7, 3 ), ( 1, 4 ), ( 8, 4 ), ( 1, 5 ), ( 8, 5 ), ( 2, 6 ), ( 7, 6 ), ( 3, 7 ), ( 6, 7 ), ( 4, 8 ), ( 5, 8 ) ]



-- VIEW


view : Model -> Html Msg
view { generation, params, board } =
    div []
        [ viewToolbar generation params
        , viewBoard board
        ]


viewToolbar : Int -> Params -> Html Msg
viewToolbar generation { autorunSpeed } =
    header []
        [ span [] [ text ("Generation #" ++ String.fromInt generation) ]
        , button [ Events.onClick Evolve, Attr.disabled (autorunSpeed > 0) ]
            [ text "Evolve!" ]
        , viewSpeedSelector autorunSpeed
        ]


viewSpeedSelector : Int -> Html Msg
viewSpeedSelector speed =
    div []
        [ label [] [ text "Autorun speed" ]
        , viewSpeedRadioButton 0 (speed == 0)
        , viewSpeedRadioButton 1 (speed == 1)
        , viewSpeedRadioButton 2 (speed == 2)
        , viewSpeedRadioButton 5 (speed == 5)
        , viewSpeedRadioButton 10 (speed == 10)
        ]


viewSpeedRadioButton : Int -> Bool -> Html Msg
viewSpeedRadioButton value isChecked =
    label
        []
        [ input [ Attr.type_ "radio", Attr.name "autorunSpeed", Events.onInput (\_ -> SetSpeed value), Attr.checked isChecked ] []
        , text (String.fromInt value)
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        height =
            Matrix.height board

        rows =
            List.range 0 (height - 1)
                |> List.map (\r -> Matrix.getRow r board)
                |> List.map
                    (\r ->
                        case r of
                            -- should not ever happen, but if it does, fail gracefully by replacing with a row of dead cells
                            Err _ ->
                                List.repeat (Matrix.width board) Dead

                            Ok cells ->
                                Array.toList cells
                    )
    in
    rows |> List.map viewBoardRow |> table []


viewBoardRow : List Cell -> Html Msg
viewBoardRow cells =
    cells |> List.map viewCell |> tr []


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Alive ->
            td [] [ text "●" ]

        Dead ->
            td [] [ text " " ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Evolve ->
            ( { model | generation = model.generation + 1, board = evolve model.board }, Cmd.none )

        SetSpeed speed ->
            let
                params =
                    model.params

                newParams =
                    { params | autorunSpeed = speed }
            in
            ( { model | params = newParams }, Cmd.none )



-- COMMANDS AND TASKS


subscriptions : Model -> Sub Msg
subscriptions { params } =
    case params.autorunSpeed of
        0 ->
            Sub.none

        speed ->
            Time.every (1000 / toFloat speed) (\_ -> Evolve)



-- BUILD BOARD


makeBoard : Int -> Int -> List ( Int, Int ) -> Board
makeBoard width height positionsOfAliveCells =
    Matrix.generate width height (setCellType positionsOfAliveCells)


setCellType : List ( Int, Int ) -> Int -> Int -> Cell
setCellType alivePositions x y =
    if List.member ( x, y ) alivePositions then
        Alive

    else
        Dead



-- GAME LOGIC


type alias Params =
    { autorunSpeed : Int
    , showPredictions : Bool
    }


type Cell
    = Dead
    | Alive


type alias Board =
    Matrix Cell


evolveCell : Board -> Int -> Int -> Cell -> Cell
evolveCell board x y cell =
    let
        cellAliveNeighbours =
            neighbours Plane x y board |> Array.filter (\c -> c == Alive) |> Array.length
    in
    case cellAliveNeighbours of
        3 ->
            Alive

        2 ->
            cell

        _ ->
            Dead


evolve : Board -> Board
evolve board =
    board |> Matrix.indexedMap (evolveCell board)
