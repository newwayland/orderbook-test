module View.Clock exposing (displayAge, displayBirthDate, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model.Clock exposing (Clock, DateTime)
import Model.Types exposing (BirthDate)
import Update exposing (Msg(..))


view : Clock -> Html Msg
view clock =
    div []
        [ h1 [] [ Model.Clock.toDateTime clock |> dateTimeView |> text ]
        , ButtonGroup.buttonGroup
            [ ButtonGroup.large ]
            [ ButtonGroup.button
                [ Button.primary, Button.attrs [ onClick Pause ] ]
                [ span [ class "fa fa-pause" ] [] ]
            , ButtonGroup.button
                [ Button.primary, Button.attrs [ onClick NormalSpeed ] ]
                [ span [ class "fa fa-play" ] [] ]
            , ButtonGroup.button
                [ Button.primary, Button.attrs [ onClick FastSpeed ] ]
                [ span [ class "fa fa-forward" ] [] ]
            , ButtonGroup.button
                [ Button.primary, Button.attrs [ onClick FullSpeed ] ]
                [ span [ class "fa fa-fast-forward" ] [] ]
            ]
        ]


displayAge : Clock -> BirthDate -> String
displayAge clock =
    Model.Clock.age clock >> Model.Clock.yearIntToInt >> String.fromInt


displayBirthDate : Clock -> BirthDate -> String
displayBirthDate clock =
    Model.Clock.toDateTimeFromPosix clock >> dateTimeView



-- Helpers


currentSpeed : Clock -> String
currentSpeed clock =
    if Model.Clock.paused clock then
        "Paused"

    else
        Model.Clock.tickSpeed clock |> String.fromInt


dateTimeView : DateTime -> String
dateTimeView dateTime =
    let
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
