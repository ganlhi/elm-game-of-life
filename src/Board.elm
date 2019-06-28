module Board exposing (Board, evolve, generateFromList, generateFromPattern, getAliveCells, getPopulation, mergeInto, toggleCell)

import Cell exposing (Cell(..))
import Patterns
import RLE
import Utils


type alias Board =
    List Cell


getPopulation : Board -> Int
getPopulation =
    List.length << getAliveCells


getAliveCells : Board -> List Cell
getAliveCells =
    List.filter Cell.isAlive


getNeighbours : Board -> Cell -> List Cell
getNeighbours board cell =
    List.filter (Cell.neighbours cell) board


getAliveNeighbours : Board -> Cell -> List Cell
getAliveNeighbours board cell =
    let
        result =
            getNeighbours (getAliveCells board) cell
    in
    result


updateDeadCells : Board -> Board
updateDeadCells board =
    let
        aliveCells =
            getAliveCells board
                |> Utils.removeDuplicates

        deadCells =
            aliveCells
                |> List.map (surroundWithDeadCells aliveCells)
                |> List.foldr (++) []
                |> Utils.removeDuplicates
    in
    aliveCells ++ deadCells


hasCellAtPos : Board -> Cell.Position -> Bool
hasCellAtPos board pos =
    board |> List.any (\c -> pos == Cell.getPos c)


surroundWithDeadCells : List Cell -> Cell -> List Cell
surroundWithDeadCells existingCells cell =
    Cell.generateDeadNeighbours cell
        |> List.filter (\c -> not (hasCellAtPos existingCells (Cell.getPos c)))


evolveCell : Board -> Cell -> Cell
evolveCell board cell =
    let
        nbAliveNeighbours =
            getAliveNeighbours board cell |> List.length

        pos =
            Cell.getPos cell

        evolvedCell =
            case nbAliveNeighbours of
                3 ->
                    Alive pos

                2 ->
                    cell

                _ ->
                    Dead pos
    in
    evolvedCell


evolve : Board -> Board
evolve board =
    let
        newBoard =
            updateDeadCells board
    in
    newBoard
        |> List.map (evolveCell newBoard)
        |> List.filter Cell.isAlive


toggleCell : ( Int, Int ) -> Board -> Board
toggleCell pos board =
    let
        aliveCells =
            getAliveCells board
    in
    if hasCellAtPos aliveCells pos then
        List.filter (\c -> Cell.getPos c /= pos) aliveCells

    else
        Alive pos :: aliveCells


generateFromPattern : ( Int, Int ) -> String -> Board
generateFromPattern ( offsetX, offsetY ) patternName =
    Patterns.get patternName
        |> Maybe.map (\{ rle } -> rle)
        |> Maybe.map RLE.decode
        |> Maybe.withDefault []
        |> List.map (\pos -> Tuple.mapBoth ((+) offsetX) ((+) offsetY) pos)
        |> List.map (\pos -> Alive pos)


generateFromList : Int -> List Bool -> Board
generateFromList rowWidth values =
    values |> List.indexedMap (mapIndexedBoolToCell rowWidth)


mapIndexedBoolToCell : Int -> Int -> Bool -> Cell
mapIndexedBoolToCell rowWidth index isAlive =
    let
        pos =
            ( modBy rowWidth index, index // rowWidth )
    in
    if isAlive then
        Alive pos

    else
        Dead pos


mergeInto : Board -> Board -> Board
mergeInto dest src =
    let
        srcAlive =
            getAliveCells src

        srcPositions =
            List.map Cell.getPos srcAlive

        destFiltered =
            dest |> List.filter (\c -> not <| List.member (Cell.getPos c) srcPositions)
    in
    destFiltered ++ srcAlive
