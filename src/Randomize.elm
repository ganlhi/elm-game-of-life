module Randomize exposing (randomizeByDensity)

import Core exposing (Msg(..))
import Random
import Random.List exposing (shuffle)


randomizeByDensity : ( Int, Int ) -> Float -> Cmd Msg
randomizeByDensity ( width, height ) density =
    let
        size =
            width * height

        sizeAlive =
            density * toFloat size |> floor

        sizeDead =
            size - sizeAlive

        alives =
            List.repeat sizeAlive True

        deads =
            List.repeat sizeDead False
    in
    shuffle (alives ++ deads) |> Random.generate (InsertRandomPattern ( width, height ))
