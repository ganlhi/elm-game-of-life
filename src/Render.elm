module Render exposing (view)

import Board exposing (getPopulation)
import Canvas exposing (Shape)
import Cell exposing (..)
import Color
import Core exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode


view : Model -> Html Msg
view model =
    let
        factor =
            10
    in
    div
        []
        [ viewToolbar model
        , viewBoard model factor
        , viewStatusBar model
        ]


viewBoard : Model -> Int -> Html Msg
viewBoard model factor =
    let
        clickHandler : ( Int, Int ) -> Msg
        clickHandler ( x, y ) =
            let
                posX =
                    floor (toFloat x / toFloat factor)

                posY =
                    floor (toFloat y / toFloat factor)
            in
            CanvasClick ( posX, posY )
    in
    Canvas.toHtml model.viewSize
        [ onCanvasClick clickHandler ]
        [ clearScreen model.viewSize
        , renderBoard factor model.board
        ]


clearScreen : ( Int, Int ) -> Canvas.Renderable
clearScreen ( width, height ) =
    Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


renderBoard : Int -> List Cell -> Canvas.Renderable
renderBoard factor =
    Canvas.shapes [ Canvas.fill Color.black ] << renderCells factor 0.8


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



-- Custom event


onCanvasClick : (( Int, Int ) -> msg) -> Attribute msg
onCanvasClick tagger =
    Events.on "click" (Decode.map tagger mousePos)


mousePos : Decode.Decoder ( Int, Int )
mousePos =
    Decode.map2 (\x y -> ( x, y ))
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)
