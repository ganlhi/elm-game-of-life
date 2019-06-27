module Render exposing (view)

import Board exposing (getPopulation)
import Canvas exposing (Shape)
import Cell exposing (Cell(..))
import Color
import Core exposing (Model, Msg(..), Viewport)
import Html exposing (Html, button, div, fieldset, footer, legend, span, text)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode
import MouseMovement exposing (onMove)


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
    let
        cellviews =
            extractCellViews model.viewport model.board
    in
    Canvas.toHtml model.viewSize
        [ Mouse.onClick (clickHandler model.viewport.topLeft model.viewport.zoom)
        , Mouse.onDown (mouseUpDownHandler True)
        , Mouse.onUp (mouseUpDownHandler False)
        , Mouse.onLeave (\_ -> Panning False)
        , panHandler model.panning
        , Wheel.onWheel handleZoom
        ]
        [ clearScreen model.viewSize
        , renderBoard cellviews
        ]


clearScreen : ( Int, Int ) -> Canvas.Renderable
clearScreen ( width, height ) =
    Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderBoard : List CellView -> Canvas.Renderable
renderBoard =
    Canvas.shapes [ Canvas.fill Color.black ] << List.map renderCell


renderCell : CellView -> Shape
renderCell cv =
    Canvas.rect cv.pos cv.size cv.size


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



-- Data


type alias CellView =
    { pos : ( Float, Float )
    , size : Float
    }


cellView : Viewport -> Cell -> CellView
cellView { zoom, topLeft } cell =
    let
        ( left, top ) =
            topLeft

        ( x, y ) =
            Cell.getPos cell

        pos =
            ( toFloat x * zoom - left, toFloat y * zoom - top )

        size =
            zoom * 0.8
    in
    { pos = pos, size = size }


extractCellViews : Viewport -> List Cell -> List CellView
extractCellViews viewport =
    List.filter Cell.isAlive
        >> List.map (cellView viewport)



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
    ToggleCell ( posX, posY )


mouseUpDownHandler : Bool -> Mouse.Event -> Msg
mouseUpDownHandler panMode mouseEvent =
    case mouseEvent.button of
        Mouse.MiddleButton ->
            Panning panMode

        _ ->
            Noop


panHandler : Bool -> Html.Attribute Msg
panHandler panning =
    if not panning then
        Events.custom "mousemove" <| Decode.fail "not panning"

    else
        onMove (.movement >> Movement)
