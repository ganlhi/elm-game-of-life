module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..), neighbours)
import RLE
import Task
import Time



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- APP STATE


type alias Model =
    { generation : Int, params : Params, board : Board, cursor : Maybe ( Int, Int ) }


type Msg
    = Evolve
    | Reset
    | SetSpeed Int
    | ShowPredictions Bool
    | SetDrawMode Bool
    | SetGridSize ( Int, Int )
    | SetCursor ( Int, Int )
    | DrawPattern String


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    let
        width =
            50

        height =
            30

        params =
            { autorunSpeed = 0
            , showPredictions = False
            , drawMode = False
            , gridSize = ( width, height )
            }
    in
    { generation = 0
    , board = makeBoard width height
    , params = params
    , cursor = Nothing
    }



-- VIEW


view : Model -> Html Msg
view { generation, params, board, cursor } =
    div []
        [ viewToolbar generation params
        , viewPatternsButtons allPatterns
        , viewBoard board params.showPredictions cursor
        ]


viewToolbar : Int -> Params -> Html Msg
viewToolbar generation { autorunSpeed, showPredictions, gridSize, drawMode } =
    header []
        [ span [] [ text ("Generation #" ++ String.fromInt generation) ]
        , button [ Events.onClick Evolve, Attr.disabled (autorunSpeed > 0) ]
            [ text "Evolve!" ]
        , button [ Events.onClick Reset ]
            [ text "Reset" ]
        , viewSpeedSelector autorunSpeed
        , viewShowPredictionCheckbox showPredictions
        , viewDrawModeCheckbox drawMode
        , viewGridSizeControls gridSize
        ]


viewSpeedSelector : Int -> Html Msg
viewSpeedSelector speed =
    div []
        [ label [] [ text "Autorun speed" ]
        , viewSpeedRadioButton 0 (speed == 0)
        , viewSpeedRadioButton 1 (speed == 1)
        , viewSpeedRadioButton 2 (speed == 2)
        , viewSpeedRadioButton 5 (speed == 5)
        , viewSpeedRadioButton 10 (speed == 10)
        ]


viewSpeedRadioButton : Int -> Bool -> Html Msg
viewSpeedRadioButton value isChecked =
    label
        []
        [ input [ Attr.type_ "radio", Attr.name "autorunSpeed", Events.onInput (\_ -> SetSpeed value), Attr.checked isChecked ] []
        , text (String.fromInt value)
        ]


viewDrawModeCheckbox : Bool -> Html Msg
viewDrawModeCheckbox enabled =
    label []
        [ input [ Attr.type_ "checkbox", Attr.checked enabled, Events.onCheck SetDrawMode ] []
        , text "Draw mode"
        ]


viewShowPredictionCheckbox : Bool -> Html Msg
viewShowPredictionCheckbox show =
    label []
        [ input [ Attr.type_ "checkbox", Attr.checked show, Events.onCheck ShowPredictions ] []
        , text "Show predictions"
        ]


viewGridSizeControls : ( Int, Int ) -> Html Msg
viewGridSizeControls ( width, height ) =
    div []
        [ div []
            [ button [ Events.onClick (SetGridSize ( width - 1, height )) ] [ text "-" ]
            , span [] [ text ("width: " ++ String.fromInt width) ]
            , button [ Events.onClick (SetGridSize ( width + 1, height )) ] [ text "+" ]
            ]
        , div []
            [ button [ Events.onClick (SetGridSize ( width, height - 1 )) ] [ text "-" ]
            , span [] [ text ("height: " ++ String.fromInt height) ]
            , button [ Events.onClick (SetGridSize ( width, height + 1 )) ] [ text "+" ]
            ]
        ]


viewPatternsButtons : List Pattern -> Html Msg
viewPatternsButtons patterns =
    List.map (\{ name, rle } -> button [ Events.onClick (DrawPattern rle) ] [ text name ]) patterns |> div []


viewBoard : Board -> Bool -> Maybe ( Int, Int ) -> Html Msg
viewBoard board showPredictions cursor =
    let
        rows =
            board |> getBoardRowsWithFuture showPredictions
    in
    List.map (viewBoardRow cursor) rows |> table []


viewBoardRow : Maybe ( Int, Int ) -> List CellWithFutureAndPosition -> Html Msg
viewBoardRow cursor cells =
    List.map (viewCell cursor) cells |> tr []


viewCell : Maybe ( Int, Int ) -> CellWithFutureAndPosition -> Html Msg
viewCell cursor ( ( x, y ), current, future ) =
    let
        ( char, cls ) =
            case ( current, future ) of
                ( Alive, Alive ) ->
                    ( "●", "" )

                ( Dead, Dead ) ->
                    ( "", "" )

                ( Alive, Dead ) ->
                    ( "●", "prediction" )

                ( Dead, Alive ) ->
                    ( "◌", "prediction" )

        highlightCls =
            case cursor of
                Nothing ->
                    ""

                Just coords ->
                    if coords == ( x, y ) then
                        "highlight"

                    else
                        ""

        tooltip =
            String.fromInt x ++ ", " ++ String.fromInt y
    in
    td [ Attr.class (String.join " " [ cls, highlightCls ]), Attr.title tooltip, Events.onClick (SetCursor ( x, y )) ] [ text char ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel, Cmd.none )

        Evolve ->
            ( { model | generation = model.generation + 1, board = evolve model.board }, Cmd.none )

        SetSpeed speed ->
            ( { model | params = model.params |> setAutorunSpeed speed }
            , Cmd.none
            )

        ShowPredictions show ->
            ( { model | params = model.params |> setShowPredictions show }
            , Cmd.none
            )

        SetDrawMode enabled ->
            ( { model | params = model.params |> setDrawMode enabled }
            , Cmd.none
            )

        SetGridSize size ->
            let
                newParams =
                    model.params |> setGridSize size
            in
            ( { model | params = newParams, board = resize newParams.gridSize model.board }
            , Cmd.none
            )

        SetCursor cursorPos ->
            let
                board =
                    if model.params.drawMode then
                        toggleCell cursorPos model.board

                    else
                        model.board
            in
            ( { model | cursor = Just cursorPos, board = board }, Cmd.none )

        DrawPattern rle ->
            let
                positions =
                    RLE.decode rle
            in
            ( { model | board = drawCells model.board model.cursor positions }, Cmd.none )


setAutorunSpeed : Int -> Params -> Params
setAutorunSpeed speed params =
    { params | autorunSpeed = speed }


setShowPredictions : Bool -> Params -> Params
setShowPredictions show params =
    { params | showPredictions = show }


setDrawMode : Bool -> Params -> Params
setDrawMode enabled params =
    { params | drawMode = enabled }


setGridSize : ( Int, Int ) -> Params -> Params
setGridSize ( w, h ) params =
    { params | gridSize = ( clampSize w, clampSize h ) }


clampSize : Int -> Int
clampSize =
    clamp 0 100



-- COMMANDS AND TASKS


subscriptions : Model -> Sub Msg
subscriptions { params } =
    case params.autorunSpeed of
        0 ->
            Sub.none

        speed ->
            Time.every (1000 / toFloat speed) (\_ -> Evolve)



-- BUILD BOARD


makeBoard : Int -> Int -> Board
makeBoard width height =
    Matrix.repeat width height Dead


positionModel : ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
positionModel ( x0, y0 ) model =
    List.map (\( x, y ) -> ( x + x0, y + y0 )) model


resize : ( Int, Int ) -> Board -> Board
resize ( width, height ) board =
    Matrix.generate width height (copyFrom board)


copyFrom : Board -> Int -> Int -> Cell
copyFrom origin x y =
    case Matrix.get x y origin of
        Err _ ->
            Dead

        Ok cell ->
            cell


drawCells : Board -> Maybe ( Int, Int ) -> List ( Int, Int ) -> Board
drawCells board cursor positions =
    let
        topLeft =
            case cursor of
                Nothing ->
                    ( 0, 0 )

                Just xy ->
                    xy

        positionedCells =
            positionModel topLeft positions
    in
    board |> Matrix.indexedMap (mapToAliveCells positionedCells)


mapToAliveCells : List ( Int, Int ) -> Int -> Int -> Cell -> Cell
mapToAliveCells newAliveCells x y cell =
    if List.member ( x, y ) newAliveCells then
        Alive

    else
        cell



-- GAME LOGIC


type alias Params =
    { autorunSpeed : Int
    , showPredictions : Bool
    , drawMode : Bool
    , gridSize : ( Int, Int )
    }


type Cell
    = Dead
    | Alive


type alias Board =
    Matrix Cell


evolveCell : Board -> Int -> Int -> Cell -> Cell
evolveCell board x y cell =
    let
        cellAliveNeighbours =
            neighbours Plane x y board |> Array.filter (\c -> c == Alive) |> Array.length
    in
    case cellAliveNeighbours of
        3 ->
            Alive

        2 ->
            cell

        _ ->
            Dead


evolve : Board -> Board
evolve board =
    board |> Matrix.indexedMap (evolveCell board)


toggleCell : ( Int, Int ) -> Board -> Board
toggleCell ( x, y ) board =
    case Matrix.get x y board of
        Err _ ->
            board

        Ok cell ->
            Matrix.set x y (toggle cell) board


toggle : Cell -> Cell
toggle cell =
    case cell of
        Alive ->
            Dead

        Dead ->
            Alive


type alias CellWithFutureAndPosition =
    ( ( Int, Int ), Cell, Cell )


getBoardRowsWithFuture : Bool -> Board -> List (List CellWithFutureAndPosition)
getBoardRowsWithFuture showPredictions board =
    let
        currentList =
            board |> Matrix.toArray |> Array.toList

        futureList =
            if showPredictions then
                evolve board |> Matrix.toArray |> Array.toList

            else
                currentList

        currentAndFuture =
            List.map2 Tuple.pair currentList futureList

        rowLength =
            Matrix.width board

        rows =
            splitInChunks rowLength currentAndFuture
    in
    rows |> List.indexedMap mapRowWithPosition


mapRowWithPosition : Int -> List ( Cell, Cell ) -> List CellWithFutureAndPosition
mapRowWithPosition rowIndex list =
    list |> List.indexedMap (\colIndex ( cur, fut ) -> ( ( colIndex, rowIndex ), cur, fut ))



-- UTILS


splitInChunks : Int -> List a -> List (List a)
splitInChunks chunkSize list =
    case List.take chunkSize list of
        [] ->
            []

        head ->
            head :: splitInChunks chunkSize (List.drop chunkSize list)



-- MODELS


type alias Pattern =
    { name : String, rle : String }


allPatterns : List Pattern
allPatterns =
    [ { name = "Octagon2", rle = "3b2o3b$2bo2bo2b$bo4bob$o6bo$o6bo$bo4bob$2bo2bo2b$3b2o!" }
    , { name = "Glider", rle = "bob$2bo$3o!" }
    , { name = "Time bomb", rle = "bo11b2o$obo4bo6bo$7bo4bo2b$2bo2bo3bo2bo2b$2b2o6bo4b$3bo!" }
    ]
