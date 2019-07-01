module Board2 exposing (Board, cellsWorldPositions, evolve, expand, fromWorldPositions, neighbours, trim)

import Array exposing (Array)


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
    let
        { topLeft, width, cells } =
            board

        ( left, top ) =
            topLeft

        nbRows =
            Array.length cells // width

        rows =
            List.range 0 (nbRows - 1)
                |> Array.fromList
                |> Array.slice 1 -1
                |> Array.map (\rowIndex -> Array.slice (rowIndex * width + 1) ((rowIndex + 1) * width - 1) cells)

        trimmedCells =
            rows |> Array.foldr Array.append Array.empty

        trimmedBoard =
            { topLeft = ( left + 1, top + 1 )
            , width = width - 2
            , cells = trimmedCells
            }

        boardAlives =
            Array.toList cells |> List.sum

        trimmedBoardAlives =
            Array.toList trimmedCells |> List.sum
    in
    if boardAlives == trimmedBoardAlives then
        trim trimmedBoard

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
        -- |> Debug.log "offset"
        |> List.map ((+) index)
        -- |> Debug.log "neighbour index"
        |> List.map (getAt board)
        -- |> Debug.log "neighbour value"
        |> List.sum



-- |> Debug.log "neighbours number"


evolve : Board -> Board
evolve board =
    let
        expandedBoard =
            expand board

        evolvedCells =
            expandedBoard.cells
                |> Array.indexedMap (\i c -> ( c, neighboursAt expandedBoard i ))
                |> Array.map evolveCell
    in
    { expandedBoard
        | cells = evolvedCells
    }
        |> trim


evolveCell : ( Int, Int ) -> Int
evolveCell ( cell, nbNeighbours ) =
    case nbNeighbours of
        3 ->
            1

        2 ->
            cell

        _ ->
            0


cellsWorldPositions : Board -> List ( Int, Int )
cellsWorldPositions board =
    board.cells
        |> Array.indexedMap Tuple.pair
        |> Array.filter (\( _, c ) -> c == 1)
        |> Array.map Tuple.first
        |> Array.map (worldPosition board)
        |> Array.toList


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
