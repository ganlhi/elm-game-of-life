module Board2 exposing (Board, cellsWorldPositions, evolve, expand, fromWorldPositions, generateFromList, generateFromPattern, getPopulation, neighbours, toggleCell, trim)

import Array exposing (Array)
import Patterns
import RLE


type alias Board =
    { topLeft : ( Int, Int )
    , width : Int
    , cells : Array Int
    }


getAt : Board -> Int -> Int
getAt { cells } index =
    Array.get index cells
        |> Maybe.withDefault 0


indexOf : ( Int, Int ) -> Int -> ( Int, Int ) -> Int
indexOf topLeft width ( x, y ) =
    let
        ( left, top ) =
            topLeft

        ( locX, locY ) =
            ( x - left, y - top )
    in
    locY * width + locX


worldPosition : Board -> Int -> ( Int, Int )
worldPosition { topLeft, width } index =
    let
        ( left, top ) =
            topLeft

        pos =
            ( left + modBy width index, top + index // width )
    in
    pos


expand : Board -> Board
expand { topLeft, width, cells } =
    let
        newWidth =
            width + 2

        ( left, top ) =
            topLeft

        newTopLeft =
            ( left - 1, top - 1 )

        nbRows =
            Array.length cells // width

        expandedRows =
            List.range 0 (nbRows - 1)
                |> List.map (\rowIndex -> Array.slice (rowIndex * width) ((rowIndex + 1) * width) cells)
                |> List.map (\row -> Array.push 0 row |> Array.append (Array.fromList [ 0 ]))

        emptyRow =
            Array.repeat newWidth 0

        newCells =
            (emptyRow :: expandedRows)
                ++ [ emptyRow ]
                |> List.foldr Array.append Array.empty
    in
    { topLeft = newTopLeft, width = newWidth, cells = newCells }


trim : Board -> Board
trim board =
    board
        |> trimSide Top
        |> trimSide Bottom
        |> trimSide Left
        |> trimSide Right


type Side
    = Top
    | Bottom
    | Left
    | Right


extractRows : Int -> Array Int -> Array (Array Int)
extractRows width cells =
    let
        nbRows =
            Array.length cells // width
    in
    List.range 0 (nbRows - 1)
        |> Array.fromList
        |> Array.map (\index -> Array.slice (index * width) ((index + 1) * width) cells)


trimSide : Side -> Board -> Board
trimSide side board =
    let
        { topLeft, width, cells } =
            board

        ( left, top ) =
            topLeft

        trimmedBoard =
            case side of
                Top ->
                    { topLeft = ( left, top + 1 )
                    , width = width
                    , cells = cells |> Array.slice width (Array.length cells)
                    }

                Bottom ->
                    { topLeft = topLeft
                    , width = width
                    , cells = cells |> Array.slice 0 -width
                    }

                Left ->
                    { topLeft = ( left + 1, top )
                    , width = width - 1
                    , cells =
                        extractRows width cells
                            |> Array.map (Array.slice 1 width)
                            |> Array.foldr Array.append Array.empty
                    }

                Right ->
                    { topLeft = topLeft
                    , width = width - 1
                    , cells =
                        extractRows width cells
                            |> Array.map (Array.slice 0 (width - 1))
                            |> Array.foldr Array.append Array.empty
                    }

        boardAlives =
            Array.toList cells |> List.sum

        trimmedBoardAlives =
            Array.toList trimmedBoard.cells |> List.sum
    in
    if boardAlives == trimmedBoardAlives then
        trimSide side trimmedBoard

    else
        board


neighboursOffsets : Int -> List Int
neighboursOffsets width =
    [ -1
    , 1
    , -width - 1
    , -width
    , -width + 1
    , width - 1
    , width
    , width + 1
    ]


neighbours : Board -> ( Int, Int ) -> Int
neighbours board pos =
    neighboursAt board (indexOf board.topLeft board.width pos)


neighboursAt : Board -> Int -> Int
neighboursAt board index =
    neighboursOffsets board.width
        |> List.map ((+) index)
        |> List.map (getAt board)
        |> List.sum


evolve : Board -> Board
evolve board =
    let
        expandedBoard =
            expand board

        evolvedCells =
            expandedBoard.cells
                |> Array.indexedMap (\i c -> ( c, neighboursAt expandedBoard i ))
                |> Array.map evolveCell

        evolvedBoard =
            trim { expandedBoard | cells = evolvedCells }
    in
    evolvedBoard


evolveCell : ( Int, Int ) -> Int
evolveCell ( cell, nbNeighbours ) =
    case nbNeighbours of
        3 ->
            1

        2 ->
            cell

        _ ->
            0


cellsWorldPositions : Board -> List ( Int, Int, Bool )
cellsWorldPositions board =
    board.cells
        |> Array.indexedMap Tuple.pair
        |> Array.map
            (\( i, c ) ->
                let
                    ( x, y ) =
                        worldPosition board i

                    alive =
                        c == 1
                in
                ( x, y, alive )
            )
        |> Array.toList



-- |> Array.filter (\( _, c ) -> c == 1)
-- |> Array.map Tuple.first
-- |> Array.map (worldPosition board)


fromWorldPositions : List ( Int, Int ) -> Board
fromWorldPositions positions =
    let
        firstPos =
            List.head positions |> Maybe.withDefault ( 0, 0 )

        ( left, top ) =
            positions
                |> List.foldl (\( x, y ) -> Tuple.mapBoth (min x) (min y)) firstPos

        ( maxX, maxY ) =
            positions
                |> List.foldl (\( x, y ) -> Tuple.mapBoth (max x) (max y)) firstPos

        ( width, height ) =
            ( maxX + 1 - left, maxY + 1 - top )

        aliveIndices =
            positions
                |> List.map (indexOf ( left, top ) width)

        cells =
            Array.repeat (width * height) 0
                |> Array.indexedMap
                    (\i _ ->
                        if List.member i aliveIndices then
                            1

                        else
                            0
                    )
    in
    { topLeft = ( left, top )
    , width = width
    , cells = cells
    }


generateFromPattern : ( Int, Int ) -> String -> Board
generateFromPattern ( offsetX, offsetY ) patternName =
    Patterns.get patternName
        |> Maybe.map (\{ rle } -> rle)
        |> Maybe.map RLE.decode
        |> Maybe.withDefault []
        |> List.map (\pos -> Tuple.mapBoth ((+) offsetX) ((+) offsetY) pos)
        |> fromWorldPositions


generateFromList : ( Int, Int ) -> Int -> List Bool -> Board
generateFromList topLeft width values =
    let
        cells =
            values
                |> List.map
                    (\alive ->
                        if alive then
                            1

                        else
                            0
                    )
                |> Array.fromList
    in
    { topLeft = topLeft, width = width, cells = cells }


toggleCell : ( Int, Int ) -> Board -> Board
toggleCell pos board =
    let
        index =
            indexOf board.topLeft board.width pos

        value =
            Array.get index board.cells |> Maybe.withDefault 0

        cells =
            Array.set index (1 - value) board.cells
    in
    { board | cells = cells }


getPopulation : Board -> Int
getPopulation { cells } =
    cells
        |> Array.filter ((==) 1)
        |> Array.length
