module View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Model exposing (Model, tickSpeed)
import Time
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        hour =
            Time.toHour model.zone model.time
                |> String.fromInt

        minute =
            Time.toMinute model.zone model.time
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            Time.toSecond model.zone model.time
                |> String.fromInt
                |> String.padLeft 2 '0'

        currentSpeed =
            Model.tickSpeed model
                |> String.fromInt
    in
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , div []
            [ button [ onClick DecreaseSpeed ] [ text "-" ]
            , div [] [ text currentSpeed ]
            , button [ onClick IncreaseSpeed ] [ text "+" ]
            ]
        ]
