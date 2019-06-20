module RLE exposing (decode, mapMatchToGroup, splitRow)

import Regex


decode : String -> List ( Int, Int )
decode rle =
    String.split "$" rle
        |> List.indexedMap splitRow
        |> List.foldr (++) []


splitRow : Int -> String -> List ( Int, Int )
splitRow rowIndex row =
    let
        matches =
            Regex.find splitGroups row

        groups : List ( Int, Bool )
        groups =
            matches |> List.map mapMatchToGroup

        rowCells : List Bool
        rowCells =
            groups |> List.map (\( nb, alive ) -> List.repeat nb alive) |> List.foldr (++) []
    in
    rowCells
        |> List.indexedMap
            (\colIndex alive -> ( colIndex, rowIndex, alive ))
        |> List.filter (\( x, y, alive ) -> alive)
        |> List.map (\( x, y, _ ) -> ( x, y ))


mapMatchToGroup : Regex.Match -> ( Int, Bool )
mapMatchToGroup { submatches } =
    case submatches of
        _ :: (Just nb) :: (Just status) :: [] ->
            ( Maybe.withDefault 1 (String.toInt nb), status == "o" )

        _ :: Nothing :: (Just status) :: [] ->
            ( 1, status == "o" )

        _ ->
            ( 1, False )


splitGroups =
    Maybe.withDefault Regex.never <| Regex.fromString "((\\d*)([ob]))"
