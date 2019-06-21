module Cell exposing (Cell(..), Position, distanceTo, generateDeadNeighbours, getPos, isAlive, isDead, neighbours, toggle)


type alias Position =
    ( Int, Int )


type Cell
    = Alive Position
    | Dead Position


isAlive : Cell -> Bool
isAlive cell =
    case cell of
        Alive _ ->
            True

        Dead _ ->
            False


isDead : Cell -> Bool
isDead =
    not << isAlive


toggle : Cell -> Cell
toggle cell =
    case cell of
        Alive pos ->
            Dead pos

        Dead pos ->
            Alive pos


getPos : Cell -> ( Int, Int )
getPos cell =
    case cell of
        Alive pos ->
            pos

        Dead pos ->
            pos


distanceTo : Cell -> Cell -> Int
distanceTo a b =
    let
        ( xa, ya ) =
            getPos a

        ( xb, yb ) =
            getPos b

        ( xDiff, yDiff ) =
            ( abs (xa - xb), abs (ya - yb) )
    in
    max xDiff yDiff


neighbours : Cell -> Cell -> Bool
neighbours a b =
    distanceTo a b == 1


generateDeadNeighbours : Cell -> List Cell
generateDeadNeighbours cell =
    let
        offsets =
            [ ( -1, -1 )
            , ( 0, -1 )
            , ( 0, 1 )
            , ( -1, 0 )
            , ( 1, 0 )
            , ( -1, 1 )
            , ( 0, 1 )
            , ( 1, 1 )
            ]

        ( x, y ) =
            getPos cell
    in
    offsets |> List.map (\( dx, dy ) -> Dead ( dx + x, dy + y ))
