module Render exposing (view)

import Board exposing (getPopulation)
import Canvas exposing (Shape)
import Cell exposing (..)
import Color
import Core exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode


view : Model -> Html Msg
view model =
    div
        []
        [ viewToolbar model
        , viewBoard model model.zoomFactor
        , viewStatusBar model
        ]


viewBoard : Model -> Float -> Html Msg
viewBoard model zoomFactor =
    Canvas.toHtml model.viewSize
        [ Mouse.onClick (clickHandler model.zoomFactor), Wheel.onWheel handleZoom ]
        [ clearScreen model.viewSize
        , renderBoard zoomFactor model.board
        ]


clearScreen : ( Int, Int ) -> Canvas.Renderable
clearScreen ( width, height ) =
    Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderBoard : Float -> List Cell -> Canvas.Renderable
renderBoard zoomFactor =
    Canvas.shapes [ Canvas.fill Color.black ] << renderCells zoomFactor 0.8


renderCells : Float -> Float -> List Cell -> List Shape
renderCells zoomFactor coverage cells =
    List.filterMap (renderCell zoomFactor coverage) cells


renderCell : Float -> Float -> Cell -> Maybe Shape
renderCell zoomFactor coverage cell =
    case cell of
        Alive pos ->
            let
                point =
                    mapPositionToPoint zoomFactor pos

                size =
                    zoomFactor * coverage
            in
            Just (Canvas.rect point size size)

        _ ->
            Nothing


mapPositionToPoint : Float -> Cell.Position -> Canvas.Point
mapPositionToPoint zoomFactor ( x, y ) =
    ( toFloat x * zoomFactor, toFloat y * zoomFactor )


viewToolbar : Model -> Html Msg
viewToolbar model =
    div [ Attr.class "toolbar" ]
        [ button [ Events.onClick Evolve ] [ text "Evolve!" ]
        , button [ Events.onClick Reset ] [ text "Reset" ]
        , viewSpeedButtons model.simSpeed
        ]


viewSpeedButtons : Int -> Html Msg
viewSpeedButtons speed =
    fieldset []
        [ legend [] [ text "Sim. speed" ]
        , viewSpeedButton speed 0
        , viewSpeedButton speed 1
        , viewSpeedButton speed 10
        , viewSpeedButton speed 50
        ]


viewSpeedButton : Int -> Int -> Html Msg
viewSpeedButton curSpeed forSpeed =
    button [ Attr.classList [ ( "speed-btn", True ), ( "active", curSpeed == forSpeed ) ], Events.onClick (SetSpeed forSpeed) ] [ text (String.fromInt forSpeed) ]


viewStatusBar : Model -> Html Msg
viewStatusBar model =
    let
        generation =
            String.fromInt model.generation

        simSpeed =
            case model.simSpeed of
                0 ->
                    "Paused"

                s ->
                    "x" ++ String.fromInt s

        population =
            String.fromInt (Board.getPopulation model.board)
    in
    footer []
        [ span [] [ text ("Generation: " ++ generation) ]
        , span [] [ text ("Sim. speed: " ++ simSpeed) ]
        , span [] [ text ("Population: " ++ population) ]
        ]



-- Events


handleZoom : Wheel.Event -> Msg
handleZoom wheelEvent =
    if wheelEvent.deltaY > 0 then
        ZoomOut

    else
        ZoomIn


clickHandler : Float -> Mouse.Event -> Msg
clickHandler zoomFactor mouseEvent =
    let
        ( x, y ) =
            mouseEvent.offsetPos

        posX =
            floor (x / zoomFactor)

        posY =
            floor (y / zoomFactor)
    in
    CanvasClick ( posX, posY )
