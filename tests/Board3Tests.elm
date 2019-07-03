module Board3Tests exposing (suite)

import Board3 exposing (Board)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        board : Board
        board =
            [ { index = 0, cells = [ 1, 3, 4 ] }
            , { index = 1, cells = [ 0, 2, 3 ] }
            , { index = 3, cells = [ 1, 3 ] }
            ]

        mirror : Board
        mirror =
            [ { index = -1, cells = [ -1, 0, 1, 2, 3, 4, 5 ] }
            , { index = 0, cells = [ -1, 0, 2, 5 ] }
            , { index = 1, cells = [ -1, 1, 4, 5 ] }
            , { index = 2, cells = [ -1, 0, 1, 2, 3, 4, 5 ] }
            , { index = 3, cells = [ -1, 0, 2, 4, 5 ] }
            , { index = 4, cells = [ -1, 0, 1, 2, 3, 4, 5 ] }
            ]

        evolvedBoard : Board
        evolvedBoard =
            [ { index = 0, cells = [ 1, 3, 4 ] }
            , { index = 1, cells = [ 1, 2, 3, 4 ] }
            , { index = 2, cells = [ 1, 3 ] }
            ]
    in
    describe "Board3"
        [ describe "createMirror"
            [ test "makes a mirror board reprensenting dead cells" <|
                \_ -> Board3.createMirror board |> Expect.equalLists mirror
            ]
        , describe "evolve"
            [ test "computes next generation" <|
                \_ -> Board3.evolve board |> Expect.equal evolvedBoard
            ]
        ]
