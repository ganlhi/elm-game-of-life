module Board exposing (Board, Row, createMirror, evolve, generateFromList, getPopulation, toggleCell)


type alias Row =
    { index : Int
    , cells : List Int
    }


type alias Board =
    List Row


getRow : Board -> Int -> Row
getRow board index =
    board
        |> List.filter (.index >> (==) index)
        |> List.head
        |> Maybe.withDefault { index = index, cells = [] }


createMirror : Board -> Board
createMirror board =
    let
        rowIndices =
            board
                |> List.map .index

        ( minRow, maxRow ) =
            ( (List.minimum rowIndices |> Maybe.withDefault 0) - 1
            , (List.maximum rowIndices |> Maybe.withDefault 0) + 1
            )

        ( minCol, maxCol ) =
            ( (board |> List.map minOfRow |> List.minimum |> Maybe.withDefault 0) - 1
            , (board |> List.map maxOfRow |> List.maximum |> Maybe.withDefault 0) + 1
            )
    in
    List.range minRow maxRow
        |> List.map (getRow board)
        |> List.map (createMirrorRow minCol maxCol)


createMirrorRow : Int -> Int -> Row -> Row
createMirrorRow minCol maxCol row =
    let
        aliveCells =
            row.cells

        cellsRange =
            List.range minCol maxCol

        deadCells =
            cellsRange
                |> List.filter (\cell -> not (List.member cell aliveCells))
    in
    { index = row.index, cells = deadCells }


minOfRow : Row -> Int
minOfRow =
    .cells >> List.minimum >> Maybe.withDefault 0


maxOfRow : Row -> Int
maxOfRow =
    .cells >> List.maximum >> Maybe.withDefault 0


countNeighbours : Board -> Int -> Int -> Bool -> Int
countNeighbours neighboursBoard rowIndex colIndex includeSelfColumn =
    let
        filterFunction =
            if includeSelfColumn then
                \c -> abs (colIndex - c) <= 1

            else
                \c -> abs (colIndex - c) == 1
    in
    getRow neighboursBoard rowIndex
        |> .cells
        |> List.filter filterFunction
        |> List.length


evolve : Board -> Board
evolve board =
    let
        fromLivings =
            board
                |> List.map (evolveRow board True)

        fromDeads =
            board
                |> createMirror
                |> List.map (evolveRow board False)
    in
    combine fromLivings fromDeads
        |> List.filter (not << List.isEmpty << .cells)


combine : Board -> Board -> Board
combine boardA boardB =
    let
        mergeRowInto : Row -> Board -> Board
        mergeRowInto row board =
            let
                existingRow =
                    getRow board row.index

                mergedRow =
                    { row | cells = (row.cells ++ existingRow.cells) |> List.sort }
            in
            board
                |> List.map
                    (\r ->
                        if r.index == row.index then
                            mergedRow

                        else
                            r
                    )
    in
    boardA |> List.foldr mergeRowInto boardB


evolveRow : Board -> Bool -> Row -> Row
evolveRow boardForNeighbours wasAlive row =
    { row | cells = row.cells |> List.filterMap (evolveCell boardForNeighbours row.index wasAlive) }


evolveCell : Board -> Int -> Bool -> Int -> Maybe Int
evolveCell board rowIndex wasAlive colIndex =
    let
        rowNeighbours =
            countNeighbours board rowIndex colIndex False

        aboveNeighbours =
            countNeighbours board (rowIndex - 1) colIndex True

        belowNeighbours =
            countNeighbours board (rowIndex + 1) colIndex True

        nbNeighbours =
            rowNeighbours + aboveNeighbours + belowNeighbours
    in
    case nbNeighbours of
        3 ->
            Just colIndex

        2 ->
            if wasAlive then
                Just colIndex

            else
                Nothing

        _ ->
            Nothing


generateFromList : ( Int, Int ) -> Int -> List Bool -> Board
generateFromList ( left, top ) width values =
    let
        foldRows : ( Int, Int ) -> List Row -> Board
        foldRows ( rowIndex, colIndex ) rows =
            let
                existingRow =
                    rows |> List.filter (.index >> (==) rowIndex) |> List.head
            in
            case existingRow of
                Nothing ->
                    { index = rowIndex, cells = [ colIndex ] } :: rows

                Just row ->
                    let
                        updatedRow =
                            { row | cells = colIndex :: row.cells |> List.sort }
                    in
                    rows
                        |> List.map
                            (\r ->
                                if r.index == rowIndex then
                                    updatedRow

                                else
                                    r
                            )

        board : Board
        board =
            values
                |> List.indexedMap
                    (\i alive ->
                        if alive then
                            Just ( top + i // width, left + modBy width i )

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.foldr foldRows []
                |> List.sortBy .index
    in
    board


toggleCell : ( Int, Int ) -> Board -> Board
toggleCell ( colIndex, rowIndex ) board =
    let
        updateRow : Row -> Row
        updateRow row =
            if row.index /= rowIndex then
                row

            else
                { row
                    | cells =
                        if List.member colIndex row.cells then
                            List.filter ((/=) colIndex) row.cells

                        else
                            colIndex :: row.cells |> List.sort
                }
    in
    board |> List.map updateRow


getPopulation : Board -> Int
getPopulation board =
    board
        |> List.map (.cells >> List.length)
        |> List.sum
