module RLE exposing (decode, mapMatchToGroup, splitRow)

import Board exposing (Board, Row)
import Regex


decode : String -> Board
decode rle =
    String.split "$" rle |> List.indexedMap splitRow


splitRow : Int -> String -> Row
splitRow rowIndex row =
    let
        matches =
            Regex.find splitGroups row

        groups : List ( Int, Bool )
        groups =
            matches |> List.map mapMatchToGroup

        rowCells : List Int
        rowCells =
            groups
                |> List.map (\( nb, alive ) -> List.repeat nb alive)
                |> List.foldr (++) []
                |> List.indexedMap
                    (\colIndex alive ->
                        if alive then
                            Just colIndex

                        else
                            Nothing
                    )
                |> List.filterMap identity
    in
    { index = rowIndex, cells = rowCells }


mapMatchToGroup : Regex.Match -> ( Int, Bool )
mapMatchToGroup { submatches } =
    case submatches of
        _ :: (Just nb) :: (Just status) :: [] ->
            ( Maybe.withDefault 1 (String.toInt nb), status == "o" )

        _ :: Nothing :: (Just status) :: [] ->
            ( 1, status == "o" )

        _ ->
            ( 1, False )


splitGroups : Regex.Regex
splitGroups =
    Maybe.withDefault Regex.never <| Regex.fromString "((\\d*)([ob]))"
