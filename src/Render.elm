module Render exposing (view)

import Board exposing (getPopulation)
import Canvas exposing (Shape)
import Cell exposing (Cell(..))
import Color
import Core exposing (Model, Msg(..))
import Html exposing (Html, button, div, fieldset, footer, legend, span, text)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel


view : Model -> Html Msg
view model =
    div
        []
        [ viewToolbar model
        , viewBoard model
        , viewStatusBar model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Canvas.toHtml model.viewSize
        [ Mouse.onClick (clickHandler model.viewTopLeft model.zoomFactor)
        , Mouse.onDown (mouseUpDownHandler True)
        , Mouse.onUp (mouseUpDownHandler False)
        , Mouse.onLeave (\_ -> Panning Nothing)
        , Mouse.onMove (mouseMoveHandler model.panning)
        , Wheel.onWheel handleZoom
        ]
        [ clearScreen model.viewSize
        , renderBoard model.viewTopLeft model.zoomFactor model.board
        ]


clearScreen : ( Int, Int ) -> Canvas.Renderable
clearScreen ( width, height ) =
    Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderBoard : ( Float, Float ) -> Float -> List Cell -> Canvas.Renderable
renderBoard topLeft zoomFactor =
    Canvas.shapes [ Canvas.fill Color.black ] << renderCells topLeft zoomFactor 0.8


renderCells : ( Float, Float ) -> Float -> Float -> List Cell -> List Shape
renderCells topLeft zoomFactor coverage cells =
    List.filterMap (renderCell topLeft zoomFactor coverage) cells


renderCell : ( Float, Float ) -> Float -> Float -> Cell -> Maybe Shape
renderCell topLeft zoomFactor coverage cell =
    case cell of
        Alive pos ->
            let
                point =
                    mapPositionToPoint topLeft zoomFactor pos

                size =
                    zoomFactor * coverage
            in
            Just (Canvas.rect point size size)

        _ ->
            Nothing


mapPositionToPoint : ( Float, Float ) -> Float -> Cell.Position -> Canvas.Point
mapPositionToPoint ( left, top ) zoomFactor ( x, y ) =
    ( toFloat x * zoomFactor - left, toFloat y * zoomFactor - top )


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


clickHandler : ( Float, Float ) -> Float -> Mouse.Event -> Msg
clickHandler ( left, top ) zoomFactor mouseEvent =
    let
        ( x, y ) =
            mouseEvent.offsetPos

        posX =
            floor ((x + left) / zoomFactor)

        posY =
            floor ((y + top) / zoomFactor)
    in
    CanvasClick ( posX, posY )


mouseUpDownHandler : Bool -> Mouse.Event -> Msg
mouseUpDownHandler panMode mouseEvent =
    case mouseEvent.button of
        Mouse.MiddleButton ->
            if panMode then
                Panning (Just mouseEvent.clientPos)

            else
                Panning Nothing

        _ ->
            Noop


mouseMoveHandler : Maybe ( Float, Float ) -> Mouse.Event -> Msg
mouseMoveHandler panning mouseEvent =
    case panning of
        Nothing ->
            Noop

        Just _ ->
            Panning (Just mouseEvent.clientPos)
