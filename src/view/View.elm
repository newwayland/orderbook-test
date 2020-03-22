module View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Model.Clock
import Time
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        hour =
            Model.Clock.toHour model.clock
                |> String.fromInt

        minute =
            Model.Clock.toMinute model.clock
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            Model.Clock.toSecond model.clock
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , div []
            [ button [ onClick DecreaseSpeed ] [ text "-" ]
            , div [] [ currentSpeed model ]
            , button [ onClick IncreaseSpeed ] [ text "+" ]
            ]
        ]


currentSpeed : Model -> Html Msg
currentSpeed model =
    if Model.Clock.paused model.clock then
        text "Paused"

    else
        Model.Clock.tickSpeed model.clock |> String.fromInt |> text
