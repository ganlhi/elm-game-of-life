module MouseMovement exposing (EventWithMovement, onMove)

import Html
import Html.Events as Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode exposing (Decoder)


type alias EventWithMovement =
    { mouseEvent : Mouse.Event
    , movement : ( Float, Float )
    }


onMove : (EventWithMovement -> msg) -> Html.Attribute msg
onMove tag =
    let
        decoder =
            decodeWithMovement
                |> Decode.map tag
                |> Decode.map options

        options message =
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
    in
    Events.custom "mousemove" decoder


decodeWithMovement : Decoder EventWithMovement
decodeWithMovement =
    Decode.map2 EventWithMovement
        Mouse.eventDecoder
        movementDecoder


movementDecoder : Decoder ( Float, Float )
movementDecoder =
    Decode.map2 (\a b -> ( a, b ))
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)
