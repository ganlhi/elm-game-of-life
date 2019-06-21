module Render exposing (view)

import Board exposing (getPopulation)
import Canvas exposing (Shape)
import Cell exposing (..)
import Color
import Core exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events


view : Model -> Html Msg
view model =
    div []
        [ viewToolbar model
        , viewBoard model
        , viewStatusBar model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Canvas.toHtml model.viewSize
        []
        [ clearScreen model.viewSize
        , renderBoard model.board
        ]


clearScreen : ( Int, Int ) -> Canvas.Renderable
clearScreen ( width, height ) =
    Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderBoard : List Cell -> Canvas.Renderable
renderBoard =
    Canvas.shapes [ Canvas.fill Color.black ] << renderCells 10 0.8


renderCells : Int -> Float -> List Cell -> List Shape
renderCells factor coverage cells =
    List.filterMap (renderCell factor coverage) cells


renderCell : Int -> Float -> Cell -> Maybe Shape
renderCell factor coverage cell =
    case cell of
        Alive pos ->
            let
                point =
                    mapPositionToPoint factor pos

                size =
                    toFloat factor * coverage
            in
            Just (Canvas.rect point size size)

        _ ->
            Nothing


mapPositionToPoint : Int -> Cell.Position -> Canvas.Point
mapPositionToPoint factor ( x, y ) =
    ( toFloat (x * factor), toFloat (y * factor) )


viewToolbar : Model -> Html Msg
viewToolbar model =
    div [ Attr.class "toolbar" ]
        [ button [ Events.onClick Evolve ] [ text "Evolve!" ]
        ]


viewStatusBar : Model -> Html Msg
viewStatusBar model =
    footer []
        [ span [] [ text ("Generation: " ++ String.fromInt model.generation) ]
        , span [] [ text ("Population: " ++ String.fromInt (Board.getPopulation model.board)) ]
        ]
