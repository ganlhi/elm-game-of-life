module Board2Tests exposing (cellsWorldPositions, evolve, expand, fromWorldPositions, neighbours, trim)

import Array
import Board2 exposing (Board)
import Expect
import Test exposing (Test, describe, test)


expand : Test
expand =
    let
        originalBoard =
            { topLeft = ( 1, 2 )
            , width = 3
            , cells = Array.fromList [ 0, 1, 0, 1, 1, 1, 0, 1, 1 ]
            }

        expandedCells =
            [ 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 1
            , 1
            , 1
            , 0
            , 0
            , 0
            , 1
            , 1
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]

        expandedBoard =
            { topLeft = ( 0, 1 )
            , width = 5
            , cells =
                Array.fromList expandedCells
            }
    in
    test "expand a board by adding a ring of dead cells around it" <|
        \_ ->
            originalBoard |> Board2.expand |> Expect.equal expandedBoard


trim : Test
trim =
    describe "trim"
        [ test "trims a board with empty borders" <|
            \_ ->
                let
                    expandedCells =
                        [ 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 1
                        , 0
                        , 0
                        , 0
                        , 1
                        , 1
                        , 1
                        , 0
                        , 0
                        , 0
                        , 1
                        , 1
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        ]

                    expandedBoard =
                        { topLeft = ( 0, 1 )
                        , width = 5
                        , cells =
                            Array.fromList expandedCells
                        }

                    trimmedBoard =
                        { topLeft = ( 1, 2 )
                        , width = 3
                        , cells = Array.fromList [ 0, 1, 0, 1, 1, 1, 0, 1, 1 ]
                        }
                in
                expandedBoard |> Board2.trim |> Expect.equal trimmedBoard
        , test "trims a board with empty top and bottom" <|
            \_ ->
                let
                    expandedCells =
                        [ 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 1
                        , 0
                        , 1
                        , 0
                        , 1
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        , 0
                        ]

                    expandedBoard =
                        { topLeft = ( 0, 1 )
                        , width = 3
                        , cells =
                            Array.fromList expandedCells
                        }

                    trimmedBoard =
                        { topLeft = ( 0, 3 )
                        , width = 3
                        , cells = Array.fromList [ 1, 0, 1, 0, 1, 0 ]
                        }
                in
                expandedBoard |> Board2.trim |> Expect.equal trimmedBoard
        ]


neighbours : Test
neighbours =
    let
        board : Board
        board =
            { topLeft = ( 0, 0 )
            , width = 3
            , cells = Array.fromList [ 0, 1, 0, 1, 1, 1, 0, 1, 1 ]
            }
    in
    describe "neighbours"
        [ {-
             0  1  0
             1 (1) 1
             0  1  1
          -}
          test "provides number of alive neighbours" <|
            \_ ->
                Board2.neighbours board ( 1, 1 )
                    |> Expect.equal 5

        {-
           0  1  0
           1  1  1
           0  1 (1)
        -}
        , test "provide correct number of neighbours on the edges of the pattern" <|
            \_ ->
                Board2.neighbours board ( 2, 2 )
                    |> Expect.equal 3
        ]


evolve : Test
evolve =
    let
        board : Board
        board =
            { topLeft = ( 0, 0 )
            , width = 3
            , cells = Array.fromList [ 0, 1, 0, 1, 1, 1, 0, 1, 1 ]
            }
    in
    test "evolve" <|
        \_ ->
            let
                newBoard =
                    Board2.evolve board

                expectedCells =
                    Array.fromList [ 1, 1, 1, 1, 0, 0, 1, 0, 1 ]
            in
            newBoard
                |> Expect.all
                    [ Expect.equal ( 0, 0 ) << .topLeft
                    , Expect.equal 3 << .width
                    , Expect.equal expectedCells << .cells
                    ]


cellsWorldPositions : Test
cellsWorldPositions =
    describe "cellsWorldPositions"
        [ test "provide world positions of alive cells" <|
            \_ ->
                let
                    board =
                        { topLeft = ( 1, 2 )
                        , width = 3
                        , cells = Array.fromList [ 0, 1, 0, 1, 1, 1, 0, 1, 1 ]
                        }
                in
                Board2.cellsWorldPositions board
                    |> Expect.equalLists [ ( 1, 2, False ), ( 2, 2, True ), ( 3, 2, False ), ( 1, 3, True ), ( 2, 3, True ), ( 3, 3, True ), ( 1, 4, False ), ( 2, 4, True ), ( 3, 4, True ) ]
        ]


fromWorldPositions : Test
fromWorldPositions =
    describe "fromWorldPositions"
        [ test "provide board from list of world positions" <|
            \_ ->
                let
                    positions =
                        [ ( 2, 2 ), ( 1, 3 ), ( 2, 3 ), ( 3, 3 ), ( 2, 4 ), ( 3, 4 ) ]
                in
                Board2.fromWorldPositions positions
                    |> Expect.equal
                        { topLeft = ( 1, 2 )
                        , width = 3
                        , cells = Array.fromList [ 0, 1, 0, 1, 1, 1, 0, 1, 1 ]
                        }
        ]
