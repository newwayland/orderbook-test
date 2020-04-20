module View.Clock exposing (Clock, clockCardHeader, clockControlsBlock, displayAge, displayBirthDate)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card.Block as Block
import Html exposing (Html, button, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Model.Clock exposing (DateTime)
import Model.Types exposing (BirthDate)
import String.Conversions
import Update exposing (Msg(..))


type alias Clock =
    Model.Clock.Clock


clockControlsBlock : Block.Item Msg
clockControlsBlock =
    Block.custom <|
        ButtonGroup.buttonGroup
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


displayAge : Clock -> BirthDate -> String
displayAge clock =
    Model.Clock.age clock >> Model.Clock.yearIntToInt >> String.fromInt


displayBirthDate : Clock -> BirthDate -> String
displayBirthDate clock =
    Model.Clock.posixToDateTime clock >> fromDateTime


clockCardHeader : Clock -> Accordion.Header Msg
clockCardHeader clock =
    Accordion.toggle [ style "min-width" "75%" ]
        [ Button.button
            [ Button.outlinePrimary
            , Button.large
            , Button.block
            ]
            [ displayClock clock ]
        ]
        |> Accordion.header []



-- Helpers


displayClock : Clock -> Html Msg
displayClock =
    Model.Clock.toDateTime >> fromDateTime >> text


fromDateTime : DateTime -> String
fromDateTime dateTime =
    let
        year =
            String.fromInt dateTime.year |> String.padLeft 4 '0'

        month =
            String.Conversions.fromMonth dateTime.month

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
