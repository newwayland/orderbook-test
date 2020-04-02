module View.Clock exposing (getAge, view)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Model.Clock exposing (Clock)
import Model.Types exposing (BirthDate)
import Update exposing (Msg(..))


view : Clock -> Html Msg
view clock =
    div []
        [ h1 [] [ dateTimeView clock |> text ]
        , div [] [ currentSpeed clock |> text ]
        , div []
            [ button [ onClick Pause ] [ text "⏸️" ]
            , button [ onClick NormalSpeed ] [ text "▶️" ]
            , button [ onClick FastSpeed ] [ text "⏩" ]
            , button [ onClick FullSpeed ] [ text "⏭️" ]
            ]
        ]


getAge : Clock -> BirthDate -> String
getAge clock =
    Model.Clock.age clock >> String.fromInt



-- Helpers


currentSpeed : Clock -> String
currentSpeed clock =
    if Model.Clock.paused clock then
        "Paused"

    else
        Model.Clock.tickSpeed clock |> String.fromInt


dateTimeView : Clock -> String
dateTimeView clock =
    let
        dateTime =
            Model.Clock.toDateTime clock

        year =
            String.fromInt dateTime.year |> String.padLeft 4 '0'

        month =
            Model.Clock.toDisplayMonth dateTime.month

        day =
            String.fromInt dateTime.day |> String.padLeft 2 ' '

        hour =
            zeroPadTime dateTime.hour

        minute =
            zeroPadTime dateTime.minute

        second =
            zeroPadTime dateTime.second

        displayTime =
            [ hour, minute, second ] |> String.join ":"
    in
    [ day, month, year, displayTime ] |> String.join " "


zeroPadTime : Int -> String
zeroPadTime =
    String.fromInt >> String.padLeft 2 '0'
