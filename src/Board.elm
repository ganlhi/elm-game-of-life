module Board exposing (Board, evolve, generateFromPattern)

import Cell exposing (Cell(..))
import Patterns exposing (..)
import RLE


type alias Board =
    List Cell


getAliveCells : Board -> List Cell
getAliveCells =
    List.filter Cell.isAlive


getDeadCells : Board -> List Cell
getDeadCells =
    List.filter Cell.isDead


getNeighbours : Board -> Cell -> List Cell
getNeighbours board cell =
    List.filter (Cell.neighbours cell) board


getAliveNeighbours : Board -> Cell -> List Cell
getAliveNeighbours board =
    getNeighbours (getAliveCells board)


updateDeadCells : Board -> Board
updateDeadCells board =
    let
        aliveCells =
            getAliveCells board
    in
    aliveCells
        |> List.map (surroundWithDeadCells aliveCells)
        |> List.foldr (++) []


surroundWithDeadCells : List Cell -> Cell -> List Cell
surroundWithDeadCells existingCells cell =
    let
        deadNeighbours =
            Cell.generateDeadNeighbours cell
                |> List.filter (\c -> List.member c existingCells)
    in
    cell :: deadNeighbours


evolveCell : Board -> Cell -> Cell
evolveCell board cell =
    let
        nbAliveNeighbours =
            getAliveNeighbours board cell |> List.length

        pos =
            Cell.getPos cell
    in
    case nbAliveNeighbours of
        3 ->
            Alive pos

        2 ->
            cell

        _ ->
            Dead pos


evolve : Board -> Board
evolve board =
    let
        newBoard =
            updateDeadCells board
    in
    newBoard
        |> List.map (evolveCell newBoard)


generateFromPattern : String -> Board
generateFromPattern patternName =
    Patterns.get patternName
        |> Maybe.map (\{ rle } -> rle)
        |> Maybe.map RLE.decode
        |> Maybe.withDefault []
        |> List.map (\pos -> Alive pos)
