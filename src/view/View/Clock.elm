module View.Clock exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Model.Clock exposing (Clock)
import Time
import Update exposing (Msg(..))


view : Clock -> Html Msg
view clock =
    let
        hour =
            Model.Clock.toHour clock
                |> String.fromInt

        minute =
            Model.Clock.toMinute clock
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            Model.Clock.toSecond clock
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , div []
            [ button [ onClick DecreaseSpeed ] [ text "-" ]
            , div [] [ currentSpeed clock ]
            , button [ onClick IncreaseSpeed ] [ text "+" ]
            ]
        ]


currentSpeed : Clock -> Html Msg
currentSpeed clock =
    if Model.Clock.paused clock then
        text "Paused"

    else
        Model.Clock.tickSpeed clock |> String.fromInt |> text
