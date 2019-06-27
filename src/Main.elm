module Main exposing (main)

import Board
import Browser
import Core exposing (Model, Msg(..))
import Render
import Time



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element { init = init, view = Render.view, update = update, subscriptions = subscriptions }



-- APP STATE


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { generation = 0
    , board = Board.generateFromPattern ( 10, 10 ) "Octagon2"
    , simSpeed = 0
    , viewSize = ( 800, 600 )
    , viewTopLeft = ( 0, 0 )
    , zoomFactor = 10
    , panning = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reset ->
            ( initialModel, Cmd.none )

        Evolve ->
            let
                newBoard =
                    Board.evolve model.board
            in
            ( { model | generation = model.generation + 1, board = newBoard }, Cmd.none )

        SetSpeed speed ->
            ( { model | simSpeed = speed }, Cmd.none )

        ZoomIn ->
            ( { model | zoomFactor = model.zoomFactor * 1.1 }, Cmd.none )

        ZoomOut ->
            ( { model | zoomFactor = model.zoomFactor * 0.9 }, Cmd.none )

        CanvasClick pos ->
            ( { model | board = Board.toggleCell pos model.board }, Cmd.none )

        Panning fromPos ->
            case ( model.panning, fromPos ) of
                ( Nothing, Nothing ) ->
                    ( model, Cmd.none )

                ( Nothing, Just _ ) ->
                    ( { model | panning = fromPos }, Cmd.none )

                ( Just _, Nothing ) ->
                    ( { model | panning = fromPos }, Cmd.none )

                ( Just oldPos, Just newPos ) ->
                    ( { model | viewTopLeft = panView model oldPos newPos, panning = Just newPos }, Cmd.none )


panView : Model -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
panView model ( oldX, oldY ) ( newX, newY ) =
    let
        computeDiff : Float -> Float -> Float
        computeDiff prev cur =
            cur - prev

        ( dx, dy ) =
            ( computeDiff oldX newX, computeDiff oldY newY )

        topLeft =
            ( Tuple.first model.viewTopLeft - dx, Tuple.second model.viewTopLeft - dy )
    in
    topLeft



-- COMMANDS & TASKS


subscriptions : Model -> Sub Msg
subscriptions { simSpeed } =
    case simSpeed of
        0 ->
            Sub.none

        speed ->
            Time.every (1000 / toFloat speed) (\_ -> Evolve)
