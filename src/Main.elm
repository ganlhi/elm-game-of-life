module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..), neighbours)



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }



-- APP STATE


type alias Model =
    { generation : Int, board : Board }


type Msg
    = Evolve


initialModel : Model
initialModel =
    { generation = 0, board = makeStartingBoard }


makeStartingBoard : Board
makeStartingBoard =
    -- as an example, generate a 10x10 board with an Octagon2 pulser in the middle
    makeBoard 10 10 [ ( 4, 1 ), ( 5, 1 ), ( 3, 2 ), ( 6, 2 ), ( 2, 3 ), ( 7, 3 ), ( 1, 4 ), ( 8, 4 ), ( 1, 5 ), ( 8, 5 ), ( 2, 6 ), ( 7, 6 ), ( 3, 7 ), ( 6, 7 ), ( 4, 8 ), ( 5, 8 ) ]



-- VIEW


view : Model -> Html Msg
view { generation, board } =
    div []
        [ viewToolbar generation
        , viewBoard board
        ]


viewToolbar : Int -> Html Msg
viewToolbar generation =
    header []
        [ span [] [ text ("Generation #" ++ String.fromInt generation) ]
        , button [ onClick Evolve ] [ text "Evolve!" ]
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Evolve ->
            { model | generation = model.generation + 1, board = evolve model.board }



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
